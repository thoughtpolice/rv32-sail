/* -- NB: automatically set by build system! -- */
const rv32Ver = '@RV32_VERSION@';

/* -- escape codes -- */
const esc = `\x1b`;
const bel = `\x07`;
const clrln = `${esc}[2;K`;
const bullet = '\u{2022}';
const sgr = (...args) => `${esc}[${args.join(';')}m`;
const sgrRgbFg = (...args) => sgr(`38:2:${args.join(':')}`);
const sgrRgbBg = (...args) => sgr(`48:2:${args.join(':')}`);
const normal = sgr(0);
const bold = sgr(1);
const italic = sgr(3);
const underline = sgr(4);
const osc = (...args) => `${esc}]${args.join(';')}${bel}`;
const osc8 = (uri = '', params = '') => osc(8, params, uri);

var errcolor = sgrRgbFg(0xee, 0xb2, 0x11);

/* -- printing of basic info, command help, etc -- */

function printPrompt(io) {
    io.print(
        `${bold}` +
        `${sgrRgbFg(0x7a, 0xe9, 0x46)}rv32>` +
        `${normal} `
    );
}

function printCmdHelp(io) {
    const lines = [
        `The supported command set: `,
        ``,
        `  ${bullet} ${bold}${underline}ls${normal}, which lists the available demos`,
        `  ${bullet} ${bold}${underline}run${normal} ${underline}<FILENAME>${normal} ${underline}[OPTIONS...]${normal} runs a demo, where`,
        `    ${bullet} ${underline}<FILENAME>${normal} is the name of a demo`,
        `    ${bullet} ${underline}[OPTIONS...]${normal} are optional arguments to pass to ${bold}rv32-sail${normal}`,
        `  ${bullet} ${bold}${underline}clear${normal}, which clears the terminal`,
        `  ${bullet} ${bold}${underline}reload${normal}, which reloads the page`,
        `  ${bullet} ${bold}${underline}help${normal}, the information you're reading now`,
        ``,
    ];
    lines.forEach((line) => io.println(line));
}

function printVersion(io) {
    const htermVer = lib.resource.getData('hterm/changelog/version');
    const date = lib.resource.getData('hterm/changelog/date');
    const lines = [
        `rv32-sail: version ${bold}${rv32Ver}${normal}`,
        `hterm: version ${bold}${htermVer}${normal} (built on ${bold}${date}${normal})`,
    ];
    lines.forEach((line) => io.println(line));
}

function printDemoCmd(io) {
    const lines = [
        `For example: try ${bold}run dhrystone.elf -C debug.print_info=1${normal}`,
        ``,
        `Have fun!`,
        ``,
    ];
    lines.forEach((line) => io.println(line));
}

function checkCompat(io) {
    var errs = "\r\n";
    if (!'serviceWorker' in navigator)
        errs += `${errcolor}WARNING: Service Worker support is not present! This demo won't work${normal}\r\n`;
    if (!typeof WebAssembly == "object")
        errs += `${errcolor}WARNING: WebAssembly support is not present! This demo won't work${normal}\r\n`;

    if (errs != "\r\n") { io.println(errs); } else { io.print(errs); }
}

function initContent(io) {
    let linkify = (uri, txt) => `${underline}${bold}${osc8(uri)}${txt}${osc8()}${normal}`;

    let sailHome = linkify("https://www.cl.cam.ac.uk/~pes20/sail/", "Sail Programming Language");
    let htermHome = linkify("https://hterm.org", "hterm terminal emulator");
    let riscvHome = linkify("https://www.riscv.org", "RISC-V");
    let wasmHome = linkify("https://webassembly.org", "WebAssembly");

    let lines = [
        ``,
        `${bold}rv32-sail: a RISC-V RV32IM emulator, written in Sail${normal}`,
        ``,
        `${bold}rv32-sail${normal} is a full ${riscvHome} emulator for RV32IM in machine`,
        `(M-) mode, written using the ${sailHome}.`,
        `This is a version of the emulator's C code, compiled to`,
        `${wasmHome}, equipped with the ${htermHome}.`,
        ``,
    ];
    lines.forEach((line) => io.println(line));

    const topLine = '\u{23ba}';
    const colorTermFg = sgrRgbFg(0x7a, 0xe9, 0x46);
    const colorTermBg = sgrRgbBg(0x10, 0x37, 0x18);

    const colorRvFgR = sgrRgbFg(0x00, 175, 0xFF);
    const colorRvFgV = sgrRgbFg(175, 175, 0x00);
    const colorRvFg32 = sgrRgbFg(128, 0, 128);
    lines = [
        ``,
        `                   .--~~~~~~~~~~~~~------.`,
        `                  /--===============------\\`,
        `${colorRvFgR} ____${colorRvFgV}__     __${normal}    | |${colorTermBg}${topLine.repeat(18)}${normal}|     |`,
        `${colorRvFgR}|  _ ${colorRvFgV}\\ \\   / /${normal}    | |${colorTermBg}               ${normal}|     |`,
        `${colorRvFgR}| |_) ${colorRvFgV}\\ \\ / /${normal}     | |${colorTermBg}      ${colorTermFg}>_<      ${normal}|     |`,
        `${colorRvFgR}|  _ < ${colorRvFgV}\\ V /${normal}      | |${colorTermBg}               ${normal}|     |`,
        `${colorRvFgR}|_| \\_\\${colorRvFgV} \\_/${normal}       | |${colorTermBg}_______________${normal}|     |`,
        `${colorRvFg32} _________${normal}        |                   ::::|`,
        `${colorRvFg32}|___ /___ \\${normal}       '======================='`,
        `${colorRvFg32}  |_ \\ __) |${normal}      //-'-'-'-'-'-'-'-'-'-'-\\\\`,
        `${colorRvFg32} ___) / __/${normal}      //_'_'_'_'_'_'_'_'_'_'_'_\\\\`,
        `${colorRvFg32}|____/_____|${normal}     [-------------------------]`,
        `                 \\_________________________/`,
        ``,
    ];

    lines.forEach((line) => io.println(' '.repeat(5) + line));

    let links = [
        ['https://github.com/thoughtpolice/rv32-sail/', 'Homepage, Source Code, & Documentation'],
        ['https://github.com/thoughtpolice/rv32-sail/issues', 'Bugs & Feature Requests'],
        ['mailto:aseipp at pobox dot com', 'Contact'],
        ['https://github.com/riscv/riscv-isa-manual/releases', 'Recent RISC-V ISA Specifications (PDFs)'],
        ['https://www.cl.cam.ac.uk/~pes20/sail/', 'Sail Homepage'],
        ['https://www.cl.cam.ac.uk/~pes20/rems/', 'The REMS Project'],
    ];
    links.forEach(([uri, text]) => io.println(`  ${bullet} ${linkify(uri, text)}`));
    io.println('');

    printVersion(io);
    checkCompat(io);
    printCmdHelp(io);
};

/* -- command parsing and dispatch point -- */

function runProgram(io, demo, args) {
    io.println('Loading ' + demo + '...');
    io.println('');
    errcolor = sgrRgbFg(0xee, 0xb2, 0x11);

    var worker = new Worker('worker.js');
    worker.addEventListener('message', function (e) {
        switch (e.data.type) {
            case 'stdout': io.println(e.data.data); break;
            case 'stderr': io.println(`${errcolor}${e.data.data}${normal}`); break;
            case 'loaded': {
                // Update the printErr routine again, so that it has a distinct look
                // from load-time errors
                errcolor = `${sgrRgbFg(0xd5, 0x0f, 0x25)}`;

                // Send the message
                worker.postMessage({ 'cmd': 'exec', 'data': args });
                break;
            }
            case 'close': {
                printPrompt(io);
                break;
            }
        }
    }, false);
}

function processCommand(io, term, input) {
    if (input == '') { printPrompt(io); return; }

    var words = input.split(' ').filter(i => i); /* ignore empty '' elements */
    var cmd = words[0];

    // TODO FIXME: why doesn't smoke work?
    var validDemos = ["42", "dhrystone", "forth", "coremark"]
        .map((f) => f + '.elf');

    switch (cmd) {
        case 'clear': { term.wipeContents(); printPrompt(io); break; }
        case 'ls': { io.println(validDemos.join(' ')); printPrompt(io); break; }
        case 'reload': { location.reload(); printPrompt(io); break; }
        case 'help': {
            printCmdHelp(io);
            printPrompt(io);
            break;
        }
        case 'run': {
            if (words.length < 2) {
                io.println("INVALID COMMAND ARGUMENTS: 'run' must have the name of a demo!");
                io.println("Try 'ls' to see which demos you can run.");
                break;
            }

            var demo = words[1];
            if (!validDemos.includes(demo)) {
                io.println("INVALID DEMO NAME: '" + demo + "' does not exist!");
                io.println("Try 'ls' to see which demos you can run.");
                printPrompt(io);
                break;
            }

            var args = ["-e", "demos/" + demo];
            if (words.length > 2) args = [].concat(args, words.slice(2));

            runProgram(io, demo, args);
            break;
        }

        default:
            io.println("INVALID COMMAND WORD: '" + cmd + "'");
            printPrompt(io);
    }
}

/* -- hterm command processing, initialization -- */

var termbuf = "";
function setupHterm() {
    // We don't mark it local so people can play with it in the dev console.
    var term = new hterm.Terminal();
    term.onTerminalReady = function () {
        const io = this.io.push();

        io.onVTKeystroke = (string) => {
            switch (string) {
                case '\r':
                    io.println('');
                    processCommand(io, term, termbuf);
                    termbuf = "";
                    break;
                case '\b':
                    if (termbuf.length != 0) {
                        termbuf = termbuf.substring(0, termbuf.length - 1);
                        term.scheduleSyncCursorPosition_();
                        term.cursorLeft();
                        term.eraseToRight();
                    }
                    break;
                default:
                    termbuf += string;
                    io.print(string);
                    break;
            }
        };

        io.sendString = (string) => {
            termbuf += string;
            io.print(string);
        }

        initContent(io);
        this.setCursorVisible(true);

        this.keyboard.bindings.addBindings({
            // Allow page refreshing.
            'Ctrl-R': 'PASS',
            'Ctrl-Shift-R': 'PASS',
            // Fullscreen shortcut.
            'F11': 'PASS',
        });

        // Just enough to provide an example.
        this.contextMenu.setItems([
            ['Terminal Clear', function () { term.wipeContents(); }],
            ['Terminal Reset', function () { term.reset(); }],
        ]);

        printDemoCmd(io);
        printPrompt(io);
    };
    term.prefs_.set('backspace-sends-backspace', true);
    term.prefs_.set('cursor-blink', true);
    term.decorate(document.querySelector('#terminal'));
    term.installKeyboard();


    // Useful for console debugging.
    window.term_ = term;
}
