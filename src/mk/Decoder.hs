{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Decoder where

-- base
import           Data.Char     ( toLower )
import           Data.List     ( intercalate )
import           Data.Proxy    ( Proxy(..) )
import           GHC.TypeLits
import           Text.Printf   ( printf )

--------------------------------------------------------------------------------
-- RISC-V DSL for the Sail Instruction Decoder

--
-- Base Instruction Formats
--

type Op = Int
type F7 = Int
type F5 = Int
type F4 = Int
type F3 = Int
type F2 = Int

data BaseInstrType
  -- RV32I types
  = RTy  F7 F3 Op
  | ITy F3 Op
  | STy F3 Op
  | BTy F3 Op
  | UTy Op
  | JTy Op

  -- F/D types
  | R4Ty F2 Op

  -- A types
  | A0Ty F5 F5 F3 Op
  | A1Ty F5 F3 Op

  -- Raw types (ECALL, etc)
  | Fence F4 F5 F3 F5 Op
  | Raw Integer

type Name = String
data Instr = BaseInstr Name BaseInstrType

-- I
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

  , BaseInstr "FENCE"   (Fence 0b0000 0b00000 0b000 0b00000 0b0001111)
  , BaseInstr "FENCE.I" (Raw 0b00000000000000000001000000001111)

  , BaseInstr "ECALL"   (Raw 0b00000000000000000000000000000000)
  , BaseInstr "EBREAK"  (Raw 0b00000000000100000000000000000000)

  , BaseInstr "CSRRW"   (ITy 0b001 0b1110011)
  , BaseInstr "CSRRS"   (ITy 0b010 0b1110011)
  , BaseInstr "CSRRC"   (ITy 0b011 0b1110011)
  , BaseInstr "CSRRWI"  (ITy 0b101 0b1110011)
  , BaseInstr "CSRRSI"  (ITy 0b110 0b1110011)
  , BaseInstr "CSRRCI"  (ITy 0b111 0b1110011)
  ]

-- M
multInstrs =
  [ BaseInstr "MUL"    (RTy 0b0000001 0b000 0b0110011)
  , BaseInstr "MULH"   (RTy 0b0000001 0b001 0b0110011)
  , BaseInstr "MULHSU" (RTy 0b0000001 0b010 0b0110011)
  , BaseInstr "MULHU"  (RTy 0b0000001 0b011 0b0110011)
  , BaseInstr "DIV"    (RTy 0b0000001 0b100 0b0110011)
  , BaseInstr "DIVU"   (RTy 0b0000001 0b101 0b0110011)
  , BaseInstr "REM"    (RTy 0b0000001 0b110 0b0110011)
  , BaseInstr "REMU"   (RTy 0b0000001 0b111 0b0110011)
  ]

-- A
atomicInstrs =
  [ BaseInstr "LR.W"      (A0Ty 0b00010 0b00000 0b010 0b0101111)
  , BaseInstr "SC.W"      (A1Ty 0b00011         0b010 0b0101111)

  , BaseInstr "AMOSWAP.W" (A1Ty 0b00011         0b010 0b0101111)
  , BaseInstr "AMOADD.W"  (A1Ty 0b00001         0b010 0b0101111)
  , BaseInstr "AMOXOR.W"  (A1Ty 0b00000         0b010 0b0101111)
  , BaseInstr "AMOAND.W"  (A1Ty 0b00100         0b010 0b0101111)
  , BaseInstr "AMOOR.W"   (A1Ty 0b01100         0b010 0b0101111)
  , BaseInstr "AMOMIN.W"  (A1Ty 0b01000         0b010 0b0101111)
  , BaseInstr "AMOMAX.W"  (A1Ty 0b10000         0b010 0b0101111)
  , BaseInstr "AMOMINU.W" (A1Ty 0b11000         0b010 0b0101111)
  , BaseInstr "AMOMAXU.W" (A1Ty 0b11100         0b010 0b0101111)
  ]

-- F
floatInstrs =
  [ BaseInstr "FLW"      (ITy  0b010 0b0000111)
  , BaseInstr "FSW"      (STy  0b010 0b0100111)

  , BaseInstr "FMADD.S"  (R4Ty 0b00  0b1000011)
  , BaseInstr "FMSUB.S"  (R4Ty 0b00  0b1000111)
  , BaseInstr "FNMSUB.S" (R4Ty 0b00  0b1001011)
  , BaseInstr "FNMADD.S" (R4Ty 0b00  0b1001111)
  ]

doubleInstrs =
  [ BaseInstr "FLD"      (ITy 0b011 0b0000111)
  , BaseInstr "FSD"      (STy 0b011 0b0100111)

  , BaseInstr "FMADD.D"  (R4Ty 0b01  0b1000011)
  , BaseInstr "FMSUB.D"  (R4Ty 0b01  0b1000111)
  , BaseInstr "FNMSUB.D" (R4Ty 0b01  0b1001011)
  , BaseInstr "FNMADD.D" (R4Ty 0b01  0b1001111)
  ]

--------------------------------------------------------------------------------
-- Code generator

-- constructor names can't have '.' in them
fixupName = map (\x -> if (x == '.') then '_' else x)
-- for print_insn
lowerName = map toLower

-- 32 or 16-bit encode/decode?
data EncdecTy = Base | Comp
encdecTyFunc Base = "encdec_base"
encdecTyFunc Comp = "encdec_comp"

-- create the RHS of a 'mapping clause f = LHS <-> RHS'
itypeToMapP ty =
  let i2b w x = printf ("0b%0" <> show (w :: Int) <> "b") (toInteger x) :: String
      op      = i2b 7
      is      = intercalate " @ "
  in case ty of
    RTy f7 f3 o -> is [ i2b 7 f7, "rs2", "rs1", i2b 3 f3, "rd", op o ]
    ITy f3 o    -> is [ "imm", "rs1", i2b 3 f3, "rd", op o ]
    STy f3 o    -> is [ "imm1", "rs2", "rs1", i2b 3 f3, "imm0", op o ]
    BTy f3 o    -> is [ "imm1", "rs2", "rs1", i2b 3 f3, "imm0", op o ]
    UTy o       -> is [ "imm", "rd", op o ]
    JTy o       -> is [ "imm", "rd", op o ]

    A0Ty f5 f5' f3 o ->
      is [ i2b 5 f5, "aq", "rl", i2b 5 f5', "rs1", i2b 3 f3, "rd", op o ]
    A1Ty f5 f3 o ->
      is [ i2b 5 f5, "aq", "rl", "rs2", "rs1", i2b 3 f3, "rd", op o ]

    R4Ty f2 o   -> is [ "rs3", i2b 2 f2, "rs2", "rs1", "frm", "rd", op o ]

    Fence a b c d o ->
      is [ i2b 4 a, "p", "s", i2b 5 b, i2b 3 c, i2b 5 d, op o ]
    Raw o       -> i2b 32 o

-- create the LHS of a 'mapping clause f = LHS <-> RHS'
itypeToCtor (fixupName -> nam) ty = nam <> itypeToCtorTy ty

itypeToCtorTy ty = case ty of
  RTy _ _ _   -> "(rs2, rs1, rd)"
  ITy _ _     -> "(imm, rs1, rd)"
  STy _ _     -> "(imm1, rs2, rs1, imm0)"
  BTy _ _     -> "(imm1, rs2, rs1, imm0)"
  UTy _       -> "(imm, rd)"
  JTy _       -> "(imm, rd)"

  A0Ty _ _ _ _ -> "(aq, rl, rs1, rd)"
  A1Ty _ _ _   -> "(aq, rl, rs2, rs1, rd)"

  R4Ty _ _    -> "(rs3, rs2, rs1, frm, rd)"

  Fence _ _ _ _ _ -> "(p, s)"
  Raw _       -> "()"

--
-- print_insn function
--

genPrintInsnHead (fixupName -> nam) ty =
  "function clause print_insn " <> nam <> itypeToCtorTy ty

genPrintInsnBody (lowerName -> nam) ty = start <> " ^-^ " <> body
  where
    quote x = "\"" <> x <> " \""
    commafy  = intercalate " ^-^ \", \" ^-^ "
    parenfiy x = "\"(\" ^-^ " <> x <> " ^-^ \")\""

    (start, body) = case ty of
      RTy _ _ _   -> (quote nam, commafy [ "rd", "rs1", "rs2" ])
      ITy _ _     -> (quote nam, commafy [ "rd", "rs1", "bits_str(imm)" ])

      STy _ _     -> (quote nam, commafy [ "rs2", "bits_str(append(imm1,imm0)) ^-^ " <> parenfiy "rs1" ])

      UTy _       -> (quote nam, commafy [ "rd", "bits_str(imm)" ])

      -- TODO FIXME
      R4Ty _ _    -> (quote nam, commafy [ "rd", "rs1", "rs2", "rs3" ])

      Fence _ _ _ _ _ -> (quote nam, commafy [ "bits_str(p)", "bits_str(s)" ])
      Raw _       -> (quote nam, quote mempty)
      _           -> (quote nam, "\"\"")

{--
      BTy _ _     -> "(imm1, rs2, rs1, imm0)"
      JTy _       -> "(imm, rd)"

      A0Ty _ _ _ _ -> (quote nam, quote "(aq, rl, rs1, rd)")
      A1Ty _ _ _   -> "(aq, rl, rs2, rs1, rd)"
--}

--
-- trivial generators: 'mapping clause FOO' and 'union clause ast = FOO'
--

genAstClause (fixupName -> nam) ty = mconcat [ "union clause ast = ", nam, " : ", sty ]
  where sty = case ty of
          RTy _ _ _ -> "Rtype"
          ITy _ _   -> "Itype"
          STy _ _   -> "Stype"
          BTy _ _   -> "Btype"
          UTy _     -> "Utype"
          JTy _     -> "Jtype"

          A0Ty _ _ _ _ -> "A0type"
          A1Ty _ _ _ -> "A1type"

          R4Ty _ _  -> "R4type"

          Fence _ _ _ _ _ -> "Mtype"
          Raw _     -> "unit"

genEncClause ety nam ty = mconcat
  [ "mapping clause ", encdecTyFunc ety
  , " = "
  , itypeToCtor nam ty
  , " <-> "
  , itypeToMapP ty
  ]

--------------------------------------------------------------------------------

-- top level generator that makes things pretty
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

genPrintInsn (BaseInstr nam ty) =
  genPrintInsnHead nam ty <> " = " <> genPrintInsnBody nam ty

--------------------------------------------------------------------------------

decoderFrontend = genInstrs <> "\n"
       <> genPrint

-- every supported instruction
genInstrs
  = intercalate "\n"
  $ map genInstrDecoder
  $ allInstrs

genPrint
  = intercalate "\n"
  $ map genPrintInsn
  $ allInstrs

allInstrs
  = baseInstrs
 ++ multInstrs
 ++ atomicInstrs
 ++ floatInstrs
 ++ doubleInstrs
