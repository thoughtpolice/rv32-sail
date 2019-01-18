{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Decoder where

-- base
import           Data.List     ( intercalate )
import           Data.Maybe    ( fromMaybe )
import           GHC.TypeLits
import           Text.Printf   ( printf )

-- shake
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Classes

--------------------------------------------------------------------------------
-- RISC-V DSL for the Sail Instruction Decoder


--
-- Base Instruction Formats
--

type Op = Int
type F7 = Int
type F3 = Int

data BaseInstrType
  = RTy F7 F3 Op
  | ITy F3 Op
  | STy F3 Op
  | BTy F3 Op
  | UTy Op
  | JTy Op
  | Raw Integer

type Name = String
data Instr = BaseInstr Name BaseInstrType

baseInstrs =
  [ BaseInstr "LUI"   (UTy 0b0110111)
  , BaseInstr "AUIPC" (UTy 0b0010111)

  , BaseInstr "JAL"   (JTy 0b1101111)
  , BaseInstr "JALR"  (ITy 0b000 0b1100111)

  , BaseInstr "BEQ"   (BTy 0b000 0b1100011)
  , BaseInstr "BNE"   (BTy 0b001 0b1100011)
  , BaseInstr "BLT"   (BTy 0b100 0b1100011)
  , BaseInstr "BGE"   (BTy 0b101 0b1100011)
  , BaseInstr "BLTU"  (BTy 0b110 0b1100011)
  , BaseInstr "BGEU"  (BTy 0b111 0b1100011)

  , BaseInstr "LB"    (ITy 0b000 0b0000011)
  , BaseInstr "LH"    (ITy 0b001 0b0000011)
  , BaseInstr "LW"    (ITy 0b010 0b0000011)
  , BaseInstr "LBU"   (ITy 0b100 0b0000011)
  , BaseInstr "LHU"   (ITy 0b101 0b0000011)

  , BaseInstr "SB"    (STy 0b000 0b0100011)
  , BaseInstr "SH"    (STy 0b001 0b0100011)
  , BaseInstr "SW"    (STy 0b010 0b0100011)

  , BaseInstr "ADDI"  (ITy 0b000 0b0010011)
  , BaseInstr "SLTI"  (ITy 0b010 0b0010011)
  , BaseInstr "SLTIU" (ITy 0b011 0b0010011)
  , BaseInstr "XORI"  (ITy 0b100 0b0010011)
  , BaseInstr "ORI"   (ITy 0b110 0b0010011)
  , BaseInstr "ANDI"  (ITy 0b111 0b0010011)

  , BaseInstr "SLLI"  (RTy 0b0000000 0b001 0b0010011)
  , BaseInstr "SRLI"  (RTy 0b0000000 0b101 0b0010011)
  , BaseInstr "SRAI"  (RTy 0b0100000 0b101 0b0010011)

  , BaseInstr "ADD"   (RTy 0b0000000 0b000 0b0110011)
  , BaseInstr "SUB"   (RTy 0b0100000 0b000 0b0110011)
  , BaseInstr "SLL"   (RTy 0b0000000 0b001 0b0110011)
  , BaseInstr "SLT"   (RTy 0b0000000 0b010 0b0110011)
  , BaseInstr "SLTU"  (RTy 0b0000000 0b011 0b0110011)
  , BaseInstr "XOR"   (RTy 0b0000000 0b100 0b0110011)
  , BaseInstr "SRL"   (RTy 0b0000000 0b101 0b0110011)
  , BaseInstr "SRA"   (RTy 0b0100000 0b101 0b0110011)
  , BaseInstr "OR"    (RTy 0b0000000 0b110 0b0110011)
  , BaseInstr "AND"   (RTy 0b0000000 0b111 0b0110011)

  --, BaseInstr "FENCE"   () TODO FIXME
  , BaseInstr "FENCE_I" (Raw 0b00000000000000000001000000001111)

  , BaseInstr "ECALL"   (Raw 0b00000000000000000000000000000000)
  , BaseInstr "EBREAK"  (Raw 0b00000000000100000000000000000000)

  , BaseInstr "CSRRW"   (ITy 0b001 0b1110011)
  , BaseInstr "CSRRS"   (ITy 0b010 0b1110011)
  , BaseInstr "CSRRC"   (ITy 0b011 0b1110011)
  , BaseInstr "CSRRWI"  (ITy 0b101 0b1110011)
  , BaseInstr "CSRRSI"  (ITy 0b110 0b1110011)
  , BaseInstr "CSRRCI"  (ITy 0b111 0b1110011)
  ]

data EncdecTy = Base | Comp

encdecTyFunc Base = "encdec_base"
encdecTyFunc Comp = "encdec_comp"

itypeToMapP ty =
  let i2b w x = printf ("0b%0" <> show w <> "b") x :: String
      op      = i2b 7
      is      = intercalate " @ "
  in case ty of
    RTy f7 f3 o -> is [ i2b 7 f7, "rs2", "rs1", i2b 3 f3, "rd", op o ]
    ITy f3 o    -> is [ "imm", "rs1", i2b 3 f3, "rd", op o ]
    STy f3 o    -> is [ "imm1", "rs2", "rs1", i2b 3 f3, "imm0", op o ]
    BTy f3 o    -> is [ "imm1", "rs2", "rs1", i2b 3 f3, "imm0", op o ]
    UTy o       -> is [ "imm", "rd", op o ]
    JTy o       -> is [ "imm", "rd", op o ]
    Raw o       -> i2b 32 o

itypeToCtor nam ty = case ty of
  RTy f7 f3 o -> nam <> "(rs2, rs1, rd)"
  ITy f3 o    -> nam <> "(imm, rs1, rd)"
  STy f3 o    -> nam <> "(imm1, rs2, rs1, imm0)"
  BTy f3 o    -> nam <> "(imm1, rs2, rs1, imm0)"
  UTy o       -> nam <> "(imm, rd)"
  JTy o       -> nam <> "(imm, rd)"
  Raw o       -> nam <> "()"

genAstClause nam ty = mconcat [ "union clause ast = ", nam, " : ", sty ]
  where sty = case ty of
          RTy _ _ _ -> "Rtype"
          ITy _ _   -> "Itype"
          STy _ _   -> "Stype"
          BTy _ _   -> "Btype"
          UTy _     -> "Utype"
          JTy _     -> "Jtype"
          Raw _     -> "unit"

genEncClause ety nam ty = mconcat
  [ "mapping clause ", encdecTyFunc ety
  , " = "
  , itypeToCtor nam ty
  , " <-> "
  , itypeToMapP ty
  ]

genInstrDecoder (BaseInstr nam ty) =
  let commentPro = "/* -- " <> nam <> " encoding "
      commentEp  = " */"
      size       = length (commentPro <> commentEp)
      fill       = replicate (80 - size) '-'
  in mconcat
       [ "/* " <> replicate 74 '-' <> " */", "\n"
       , commentPro <> fill <> commentEp, "\n\n"
       , genAstClause nam ty, "\n"
       , genEncClause Base nam ty, "\n"
       ]

genBaseInstrs = intercalate "\n" (map genInstrDecoder baseInstrs)
