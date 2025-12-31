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

// Render LaTeX formula to an element using math.js toTex()
function renderLatex(elementId, formula) {
    const element = document.getElementById(elementId);
    if (!element) return;
    try {
        const mathJsFormula = convertFormula(formula);
        const node = math.parse(mathJsFormula);
        let latex = node.toTex();
        latex = wrapLatexWithLineBreaks(latex);
        katex.render(latex, element, {
            throwOnError: false,
            displayMode: true
        });
    } catch (e) {
        console.error('LaTeX render error:', e);
        element.textContent = formula;
    }
}
