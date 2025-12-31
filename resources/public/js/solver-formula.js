/**
 * Formula conversion and LaTeX rendering
 */

// Convert formula from symbolic regression format to math.js format
function convertFormula(formula) {
    let converted = formula
        .replace(/Log\(/g, 'log(')
        .replace(/Exp\(/g, 'exp(')
        .replace(/Sqrt\(/g, 'sqrt(')
        .replace(/Abs\(/g, 'abs(')
        .replace(/Asin\(/g, 'asin(')
        .replace(/ArcSin\(/g, 'asin(')
        .replace(/Acos\(/g, 'acos(')
        .replace(/ArcCos\(/g, 'acos(')
        .replace(/Atan\(/g, 'atan(')
        .replace(/ArcTan\(/g, 'atan(')
        .replace(/Sinh\(/g, 'sinh(')
        .replace(/Cosh\(/g, 'cosh(')
        .replace(/Tanh\(/g, 'tanh(')
        .replace(/Sec\(/g, 'sec(')
        .replace(/Csc\(/g, 'csc(')
        .replace(/Sin\(/g, 'sin(')
        .replace(/Cos\(/g, 'cos(')
        .replace(/Tan\(/g, 'tan(')
        .replace(/\bPi\b/g, 'pi')
        .replace(/\bE\b/g, 'e');
    return converted;
}

// Add line breaks to long LaTeX formulas
function wrapLatexWithLineBreaks(latex, termsPerLine = 2, maxLineLength = 5) {
    let depth = 0;
    let termCount = 0;
    let lineLength = 0;
    let result = '';
    let i = 0;

    function addLineBreak() {
        result += ' \\\\ ';
        termCount = 0;
        lineLength = 0;
    }

    while (i < latex.length) {
        const char = latex[i];

        if (char === '{' || char === '(' || char === '[') {
            depth++;
            result += char;
            lineLength++;
            i++;
        } else if (char === '}' || char === ')' || char === ']') {
            depth--;
            result += char;
            lineLength++;
            i++;
        } else if (latex.slice(i, i + 5) === '\\left') {
            depth++;
            result += '\\left';
            lineLength += 5;
            i += 5;
        } else if (latex.slice(i, i + 6) === '\\right') {
            depth--;
            result += '\\right';
            lineLength += 6;
            i += 6;
        } else if (depth === 0 && (char === '+' || (char === '-' && i > 0))) {
            termCount++;
            if (termCount >= termsPerLine || lineLength >= maxLineLength) {
                addLineBreak();
            }
            result += char;
            lineLength++;
            i++;
        } else {
            result += char;
            lineLength++;
            i++;
        }
    }

    if (result.includes('\\\\')) {
        return '\\begin{aligned} ' + result + ' \\end{aligned}';
    }
    return result;
}

// Copy LaTeX source to clipboard
function copyLatex(elementId) {
    const el = document.getElementById(elementId);
    if (!el || !el.dataset.latex) return;
    navigator.clipboard.writeText(el.dataset.latex).then(() => {
        // Brief visual feedback
        const btn = el.querySelector('.latex-copy-btn');
        if (btn) {
            btn.classList.add('text-white');
            setTimeout(() => btn.classList.remove('text-white'), 200);
        }
    });
}

// Render LaTeX formula to an element using math.js toTex()
function renderLatex(elementId, formula) {
    const element = document.getElementById(elementId);
    if (!element) return;
    try {
        const mathJsFormula = convertFormula(formula);
        const node = math.parse(mathJsFormula);
        let latex = node.toTex();
        const wrappedLatex = wrapLatexWithLineBreaks(latex);

        // Store raw LaTeX source for copying
        element.dataset.latex = latex;

        // Create wrapper with copy button
        element.innerHTML = '';
        element.classList.add('group', 'relative');

        const latexContainer = document.createElement('div');
        katex.render(wrappedLatex, latexContainer, {
            throwOnError: false,
            displayMode: true
        });
        element.appendChild(latexContainer);

        // Add copy button
        const copyBtn = document.createElement('button');
        copyBtn.className = 'latex-copy-btn absolute right-0 top-0 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white';
        copyBtn.title = 'Copy LaTeX source';
        copyBtn.onclick = () => copyLatex(elementId);
        copyBtn.innerHTML = `
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
            </svg>
        `;
        element.appendChild(copyBtn);
    } catch (e) {
        console.error('LaTeX render error:', e);
        element.textContent = formula;
    }
}
