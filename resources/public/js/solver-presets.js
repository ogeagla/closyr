/**
 * Dataset presets and generation
 */

// Formula generators for each dataset type
const formulaGenerators = {
    'quadratic': x => x * x,
    'cubic': x => x * x * x,
    'linear': x => 2 * x + 1,
    'sine': x => Math.sin(x),
    'exponential': x => Math.exp(x),
    'nguyen4': x => Math.pow(x, 6) + Math.pow(x, 5) + Math.pow(x, 4) + Math.pow(x, 3) + Math.pow(x, 2) + x,
    'nguyen5': x => Math.sin(x * x) * Math.cos(x) - 1,
    'feynman-lorentz': x => 1 / Math.sqrt(1 - x * x),
    'feynman-wave': x => Math.sin(x)
};

// Update the number display when slider changes
function updatePointsValue(val) {
    document.getElementById('num-points').value = val;
}

// Regenerate data for the current preset with the new number of points
function regeneratePreset() {
    const select = document.getElementById('preset-select');
    const presetId = select.value;
    if (!presetId) return;

    const option = select.options[select.selectedIndex];
    const formula = option.dataset.formula;

    if (formula && formulaGenerators[formula]) {
        const numPoints = parseInt(document.getElementById('num-points').value) || 20;
        const min = parseFloat(option.dataset.min);
        const max = parseFloat(option.dataset.max);

        const xs = linspace(min, max, numPoints);
        const ys = xs.map(formulaGenerators[formula]);

        document.getElementById('xs').value = xs.map(x => x.toFixed(4)).join(', ');
        document.getElementById('ys').value = ys.map(y => y.toFixed(6)).join(', ');

        initDataEditorChart();
    }
}

// Load preset dataset
function loadPreset(presetId) {
    if (!presetId) {
        document.getElementById('points-container').classList.add('hidden');
        return;
    }

    const select = document.getElementById('preset-select');
    const option = select.options[select.selectedIndex];
    const formula = option.dataset.formula;

    const pointsContainer = document.getElementById('points-container');
    if (formula && formulaGenerators[formula]) {
        pointsContainer.classList.remove('hidden');
        regeneratePreset();
    } else {
        pointsContainer.classList.add('hidden');
        let xs = option.dataset.xs;
        let ys = option.dataset.ys;

        xs = parseArrayString(xs);
        ys = parseArrayString(ys);

        document.getElementById('xs').value = xs;
        document.getElementById('ys').value = ys;

        initDataEditorChart();
    }
}
