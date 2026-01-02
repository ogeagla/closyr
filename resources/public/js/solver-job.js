/**
 * Job control (start, stop, pause, SSE)
 */

// Current job state
let currentJobId = null;
let currentEventSource = null;
let isPaused = false;

// Input data storage
let inputXs = [];
let inputYs = [];

// Toggle pause/resume
async function togglePause() {
    if (!currentJobId) return;

    const endpoint = isPaused ? 'resume' : 'pause';

    try {
        const response = await fetch('/api/jobs/' + currentJobId + '/' + endpoint, {
            method: 'POST'
        });
        const data = await response.json();

        if (data.error) {
            console.error('Error toggling pause:', data.error);
            return;
        }

        isPaused = !isPaused;
        updatePauseButton();
    } catch (err) {
        console.error('Error toggling pause:', err);
    }
}

// Update pause button text based on state
function updatePauseButton() {
    const pauseBtn = document.getElementById('pause-btn');
    const pauseBtnText = document.getElementById('pause-btn-text');
    const startBtnText = document.getElementById('start-btn-text');

    if (isPaused) {
        pauseBtnText.textContent = 'Resume';
        pauseBtn.classList.remove('bg-yellow-600', 'hover:bg-yellow-700');
        pauseBtn.classList.add('bg-green-600', 'hover:bg-green-700');
        startBtnText.innerHTML = `
            <svg class="h-5 w-5 text-white inline mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 9v6m4-6v6m7-3a9 9 0 11-18 0 9 9 0 0118 0z"/>
            </svg>
            Paused
        `;
    } else {
        pauseBtnText.textContent = 'Pause';
        pauseBtn.classList.remove('bg-green-600', 'hover:bg-green-700');
        pauseBtn.classList.add('bg-yellow-600', 'hover:bg-yellow-700');
        startBtnText.innerHTML = `
            <svg class="animate-spin -ml-1 mr-2 h-5 w-5 text-white inline" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
            Running...
        `;
    }
}

// Stop the current job
async function stopJob() {
    if (!currentJobId) return;

    try {
        const response = await fetch('/api/jobs/' + currentJobId + '/stop', {
            method: 'POST'
        });
        const data = await response.json();

        if (data.error) {
            console.error('Error stopping job:', data.error);
        }
    } catch (err) {
        console.error('Error stopping job:', err);
    }
}

// Reset UI to ready state
function resetUI() {
    const startBtn = document.getElementById('start-btn');
    const pauseBtn = document.getElementById('pause-btn');
    const stopBtn = document.getElementById('stop-btn');
    const startBtnText = document.getElementById('start-btn-text');
    const pauseBtnText = document.getElementById('pause-btn-text');

    startBtn.disabled = false;
    startBtnText.textContent = 'Find Formula';
    pauseBtn.classList.add('hidden');
    pauseBtn.classList.remove('bg-green-600', 'hover:bg-green-700');
    pauseBtn.classList.add('bg-yellow-600', 'hover:bg-yellow-700');
    pauseBtnText.textContent = 'Pause';
    stopBtn.classList.add('hidden');
    currentJobId = null;
    isPaused = false;

    if (currentEventSource) {
        currentEventSource.close();
        currentEventSource = null;
    }
}

// Show running state
function showRunningState() {
    const startBtn = document.getElementById('start-btn');
    const pauseBtn = document.getElementById('pause-btn');
    const stopBtn = document.getElementById('stop-btn');
    const startBtnText = document.getElementById('start-btn-text');

    disposeFitChart();

    startBtn.disabled = true;
    startBtnText.innerHTML = `
        <svg class="animate-spin -ml-1 mr-2 h-5 w-5 text-white inline" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
        </svg>
        Running...
    `;
    pauseBtn.classList.remove('hidden');
    stopBtn.classList.remove('hidden');
    isPaused = false;
}

// Handle CSV file upload
async function handleCsvUpload(input) {
    const file = input.files[0];
    if (!file) return;

    const content = await file.text();

    try {
        const response = await fetch('/api/upload-csv', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({content: content})
        });

        const data = await response.json();

        if (data.error) {
            alert('Error parsing CSV: ' + data.error);
            return;
        }

        document.getElementById('xs').value = data.xs.join(', ');
        document.getElementById('ys').value = data.ys.join(', ');

        // Clear dataset name since data came from CSV
        clearDatasetName();

        initDataEditorChart();
        input.value = '';
    } catch (err) {
        alert('Error uploading CSV: ' + err.message);
    }
}

// Submit the solver form
function submitSolverForm(evt) {
    evt.preventDefault();

    const xs = document.getElementById('xs').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));
    const ys = document.getElementById('ys').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));

    inputXs = xs;
    inputYs = ys;

    const config = {
        iterations: parseInt(document.getElementById('iterations').value) || 20,
        population: parseInt(document.getElementById('population').value) || 50,
        maxLeafs: parseInt(document.getElementById('max-leafs').value) || 40
    };

    const seedValue = document.getElementById('seed').value;
    if (seedValue) {
        config.seed = parseInt(seedValue);
    }

    // Add adaptive mode and quiet logs settings
    const adaptiveModeEl = document.getElementById('adaptive-mode');
    const quietLogsEl = document.getElementById('quiet-logs');
    config.adaptiveMode = adaptiveModeEl && adaptiveModeEl.getAttribute('aria-checked') === 'true';
    config.quietLogs = !(quietLogsEl && quietLogsEl.getAttribute('aria-checked') === 'true'); // Inverted: "Verbose Logging" toggle

    // Add mutations blacklist if any mutations are excluded
    const blacklist = getMutationsBlacklist();
    if (blacklist.length > 0) {
        config.mutationsBlacklist = blacklist;
    }

    showRunningState();

    fetch('/api/solve', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({xs: xs, ys: ys, config: config})
    })
    .then(response => response.json())
    .then(data => {
        if (data.jobId) {
            currentJobId = data.jobId;
            setupSSEConnection(data.jobId);
        } else if (data.error) {
            resetUI();
            document.getElementById('results').innerHTML = `
                <div class="text-red-400">
                    <div class="font-semibold mb-2">Error</div>
                    <div>${data.error}</div>
                </div>
            `;
        }
    })
    .catch(err => {
        resetUI();
        document.getElementById('results').innerHTML = `
            <div class="text-red-400">
                <div class="font-semibold mb-2">Error</div>
                <div>${err.message}</div>
            </div>
        `;
    });
}

// Set up SSE connection for real-time progress updates
function setupSSEConnection(jobId) {
    const resultsDiv = document.getElementById('results');
    const datasetName = getSelectedDatasetName();
    const datasetBadge = datasetName
        ? `<span class="px-2 py-1 text-xs bg-blue-600 text-white rounded">${datasetName}</span>`
        : '';

    resultsDiv.innerHTML = `
        <div class="flex items-center space-x-3 mb-4">
            <svg class="animate-spin h-5 w-5 text-blue-400" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
            <span class="text-blue-400">Solver running...</span>
            ${datasetBadge}
        </div>
        <div id="progress-info" class="text-sm text-gray-400">
            Waiting for first update...
        </div>
        <div id="formula-latex" class="mt-2 p-2 bg-gray-900 rounded-md text-sm hidden overflow-x-auto"></div>
        <div id="fit-chart" class="mt-4" style="width: 100%; height: 300px;"></div>
    `;

    const eventSource = new EventSource('/api/jobs/' + jobId + '/events');
    currentEventSource = eventSource;

    eventSource.addEventListener('progress', function(e) {
        const data = JSON.parse(e.data);
        const percent = Math.round((data.iteration / data['total-iterations']) * 100);
        document.getElementById('progress-info').innerHTML = `
            <div class="mb-2">
                <div class="flex justify-between text-sm mb-1">
                    <span>Progress</span>
                    <span>${data.iteration} / ${data['total-iterations']}</span>
                </div>
                <div class="w-full bg-gray-700 rounded-full h-2">
                    <div class="bg-blue-500 h-2 rounded-full transition-all duration-300" style="width: ${percent}%"></div>
                </div>
            </div>
            <div class="mt-4">
                <div class="text-gray-300 text-sm mb-1">Current Best Formula:</div>
                <div class="group relative formula-display text-green-400 text-sm">
                    <span id="current-formula">${data['best-formula']}</span>
                    <button onclick="copyFormula('current-formula')" class="absolute right-0 top-0 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white" title="Copy to clipboard">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                        </svg>
                    </button>
                </div>
            </div>
            <div class="mt-2 text-sm">
                <span class="text-gray-400">Score:</span>
                <span class="text-white">${data['best-score'].toFixed(6)}</span>
            </div>
        `;

        renderFitChart(data['best-formula'], false);

        const latexEl = document.getElementById('formula-latex');
        if (latexEl) {
            latexEl.classList.remove('hidden');
            renderLatex('formula-latex', data['best-formula']);
        }
    });

    eventSource.addEventListener('complete', function(e) {
        eventSource.close();
        resetUI();
        const data = JSON.parse(e.data);

        saveToHistory(data);

        const datasetName = getSelectedDatasetName();
        const datasetBadge = datasetName
            ? `<span class="px-2 py-1 text-xs bg-blue-600 text-white rounded ml-2">${datasetName}</span>`
            : '';

        resultsDiv.innerHTML = `
            <div class="mb-4">
                <div class="flex items-center space-x-2 text-green-400 mb-4">
                    <svg class="h-6 w-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"/>
                    </svg>
                    <span class="font-semibold">Completed!</span>
                    ${datasetBadge}
                </div>

                <div class="mb-4">
                    <div class="text-gray-300 text-sm mb-2">Best Formula Found:</div>
                    <div class="group relative formula-display text-green-400 text-sm">
                        <span id="best-formula">${data['best-solution'].formula}</span>
                        <button onclick="copyFormula('best-formula')" class="absolute right-0 top-0 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white" title="Copy to clipboard">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                            </svg>
                        </button>
                    </div>
                    <div id="formula-latex" class="mt-2 p-2 bg-gray-900 rounded-md text-sm overflow-x-auto"></div>
                </div>

                <div class="grid grid-cols-2 gap-4 text-sm mb-6">
                    <div>
                        <span class="text-gray-400">Score:</span>
                        <span class="text-white ml-2">${data['best-solution'].score.toFixed(6)}</span>
                    </div>
                    <div>
                        <span class="text-gray-400">Complexity:</span>
                        <span class="text-white ml-2">${data['best-solution'].leafCount} nodes</span>
                    </div>
                </div>

                <div id="fit-chart" class="mt-4" style="width: 100%; height: 300px;"></div>

                <div class="mt-4">
                    <div class="text-gray-300 text-sm mb-2">Other Solutions:</div>
                    <div class="space-y-2 max-h-48 overflow-y-auto">
                        ${data['all-solutions'].slice(1, 6).map((sol, i) => `
                            <div class="bg-gray-700 rounded p-2 text-sm">
                                <div class="text-gray-200 font-mono">${sol.formula}</div>
                                <div class="text-gray-400 text-xs mt-1">Score: ${sol.score.toFixed(6)}</div>
                            </div>
                        `).join('')}
                    </div>
                </div>
            </div>
        `;

        disposeFitChart();
        renderFitChart(data['best-solution'].formula);
        renderLatex('formula-latex', data['best-solution'].formula);
    });

    eventSource.addEventListener('error', function(e) {
        eventSource.close();
        resetUI();
        if (e.data) {
            const data = JSON.parse(e.data);
            resultsDiv.innerHTML = `
                <div class="text-red-400">
                    <div class="font-semibold mb-2">Error</div>
                    <div>${data.error}</div>
                </div>
            `;
        }
    });

    eventSource.addEventListener('stopped', function(e) {
        eventSource.close();
        resetUI();

        // Parse data and save to history if we have progress data
        if (e.data) {
            const data = JSON.parse(e.data);
            if (data['last-progress']) {
                saveStoppedToHistory(data['last-progress']);
            }
        }

        resultsDiv.innerHTML = `
            <div class="text-yellow-400">
                <div class="flex items-center space-x-2 mb-2">
                    <svg class="h-6 w-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z"/>
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 10a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1h-4a1 1 0 01-1-1v-4z"/>
                    </svg>
                    <span class="font-semibold">Stopped</span>
                </div>
                <div class="text-gray-400 text-sm">The solver was stopped by user request.</div>
            </div>
        `;
    });

    eventSource.onerror = function() {
        // Connection error - might be normal end of stream
    };
}
