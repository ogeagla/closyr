/**
 * Solver initialization and event listeners
 */

// Debounced editor update
const debouncedUpdateEditor = debounce(initDataEditorChart, 300);

// Initialize on page load
document.addEventListener('DOMContentLoaded', function() {
    // Initialize the data editor chart
    initDataEditorChart();

    // Listen for changes to X and Y textareas
    document.getElementById('xs').addEventListener('input', debouncedUpdateEditor);
    document.getElementById('ys').addEventListener('input', debouncedUpdateEditor);

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

    // Stop running/paused jobs when page is closed or reloaded
    window.addEventListener('beforeunload', function() {
        if (currentJobId) {
            navigator.sendBeacon('/api/jobs/' + currentJobId + '/stop', '');
        }
    });
});
