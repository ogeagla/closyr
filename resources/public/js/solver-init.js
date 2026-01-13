/**
 * Solver initialization and event listeners
 */

// Debounced editor update
const debouncedUpdateEditor = debounce(initDataEditorChart, 30);

// Initialize on page load
document.addEventListener('DOMContentLoaded', function() {
    // Load default preset (Feynman Diffraction)
    const presetSelect = document.getElementById('preset-select');
    if (presetSelect) {
        // Find and select the option by value
        const defaultPresetId = 'feynman-diffraction';
        for (let i = 0; i < presetSelect.options.length; i++) {
            if (presetSelect.options[i].value === defaultPresetId) {
                presetSelect.selectedIndex = i;
                break;
            }
        }
        loadPreset(defaultPresetId);
    } else {
        // Fallback: just initialize the chart with whatever data is in the textareas
        initDataEditorChart();
    }

    // Listen for changes to X and Y textareas
    document.getElementById('xs').addEventListener('input', debouncedUpdateEditor);
    document.getElementById('ys').addEventListener('input', debouncedUpdateEditor);

    // Clear dataset name when user manually edits the data
    document.getElementById('xs').addEventListener('input', clearDatasetName);
    document.getElementById('ys').addEventListener('input', clearDatasetName);

    // Form submission
    document.getElementById('solver-form').addEventListener('submit', submitSolverForm);

    // Handle window resize for charts
    window.addEventListener('resize', function() {
        if (fitChart) {
            fitChart.resize();
        }
        if (dataEditorChart) {
            dataEditorChart.resize();
            setupDragHandlers();
        }
    });

    // Note: beforeunload handler for stopping all jobs is in solver-job.js
    // where it has access to activeJobs state
});
