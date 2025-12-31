/**
 * Utility functions for the solver
 */

// Debounce helper
function debounce(func, wait) {
    let timeout;
    return function(...args) {
        clearTimeout(timeout);
        timeout = setTimeout(() => func.apply(this, args), wait);
    };
}

// Copy formula to clipboard
function copyFormula(elementId) {
    const el = document.getElementById(elementId);
    if (!el) return;
    const text = el.textContent;
    navigator.clipboard.writeText(text).then(() => {
        // Brief visual feedback
        el.classList.add('text-white');
        setTimeout(() => el.classList.remove('text-white'), 200);
    });
}

// Generate evenly spaced points
function linspace(min, max, n) {
    const step = (max - min) / (n - 1);
    return Array.from({length: n}, (_, i) => min + i * step);
}

// Handle Clojure vector format [1 2 3] or JSON format [1,2,3]
function parseArrayString(str) {
    if (str.startsWith('[') && str.endsWith(']')) {
        const inner = str.slice(1, -1).trim();
        return inner.split(/[,\s]+/).filter(s => s.length > 0).join(', ');
    }
    return str;
}
