/**
 * Dataset presets and generation
 */

// Track currently selected dataset name (null if data was manually edited)
let selectedDatasetName = null;

// Get the current dataset name (or null if edited)
function getSelectedDatasetName() {
    return selectedDatasetName;
}

// Clear dataset name when data is manually edited
function clearDatasetName() {
    selectedDatasetName = null;
}

// Formula generators for each dataset type
const formulaGenerators = {
    'h-line': x => 0,
    'nguyen4': x => Math.pow(x, 6) + Math.pow(x, 5) + Math.pow(x, 4) + Math.pow(x, 3) + Math.pow(x, 2) + x,
    'nguyen5': x => Math.sin(x * x) * Math.cos(x) - 1,
    'feynman-lorentz': x => 1 / Math.sqrt(1 - x * x),
    'feynman-wave': x => Math.sin(x),
    'feynman-diffraction': x => {
        const n = 5;
        const sinHalf = Math.sin(x / 2);
        const sinNHalf = Math.sin(n * x / 2);
        if (Math.abs(sinHalf) < 1e-10) return n * n;
        return (sinNHalf * sinNHalf) / (sinHalf * sinHalf);
    },
    'feynman-planck': x => (x * x * x) / (Math.exp(x) - 1),
    'feynman-rutherford': x => {
        const sinHalf = Math.sin(x / 2);
        return 1 / Math.pow(sinHalf, 4);
    },
    'feynman-ellipse': x => {
        const e = 0.6;
        const a = 1.0;
        return (a * (1 - e * e)) / (1 + e * Math.cos(x));
    },
    'feynman-transition': x => {
        // Quantum transition probability (sinc² function) from Feynman III.9.52
        if (Math.abs(x) < 1e-10) return 1.0; // limit as x->0 is 1
        return (Math.sin(x) * Math.sin(x)) / (x * x);
    }
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

    // Find option by value (more reliable than selectedIndex)
    const option = select.querySelector(`option[value="${presetId}"]`) || select.options[select.selectedIndex];
    if (!option) return;
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
        selectedDatasetName = null;
        return;
    }

    const select = document.getElementById('preset-select');
    // Find option by value (more reliable than selectedIndex when called programmatically)
    const option = select.querySelector(`option[value="${presetId}"]`) || select.options[select.selectedIndex];
    if (!option) {
        selectedDatasetName = null;
        return;
    }
    const formula = option.dataset.formula;

    // Store the dataset name
    selectedDatasetName = option.textContent.trim();

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
