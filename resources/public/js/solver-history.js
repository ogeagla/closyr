/**
 * Job history management
 */

let jobHistory = [];

// Get display name for scoring method
function getScoringMethodDisplay(method) {
    const displays = {
        'mae-max': 'MAE + Max',
        'log-cosh': 'Log-Cosh',
        'r-squared': 'R²'
    };
    return displays[method] || method || 'MAE + Max';
}

// Save job to history
function saveToHistory(jobData, status = 'completed', scoreHistory = []) {
    const scoringMethodEl = document.getElementById('scoring-method');
    const job = {
        id: currentJobId,
        timestamp: new Date().toLocaleString(),
        status: status,
        datasetName: getSelectedDatasetName(),
        formula: jobData['best-solution'].formula,
        score: jobData['best-solution'].score,
        leafCount: jobData['best-solution'].leafCount,
        xs: [...inputXs],
        ys: [...inputYs],
        config: {
            iterations: parseInt(document.getElementById('iterations').value) || 100,
            population: parseInt(document.getElementById('population').value) || 100,
            maxLeafs: parseInt(document.getElementById('max-leafs').value) || 40,
            seed: document.getElementById('seed').value || null,
            scoringMethod: scoringMethodEl ? scoringMethodEl.value : 'mae-max'
        },
        allSolutions: jobData['all-solutions'],
        scoreHistory: scoreHistory
    };
    jobHistory.unshift(job);
    renderJobHistory();
}

// Save stopped job to history (from progress data)
function saveStoppedToHistory(progressData, scoreHistory = []) {
    if (!progressData) return;

    const scoringMethodEl = document.getElementById('scoring-method');
    const job = {
        id: currentJobId,
        timestamp: new Date().toLocaleString(),
        status: 'stopped',
        datasetName: getSelectedDatasetName(),
        formula: progressData['best-formula'],
        score: progressData['best-score'],
        leafCount: null, // Not available in progress data
        iteration: progressData['iteration'],
        totalIterations: progressData['total-iterations'],
        xs: [...inputXs],
        ys: [...inputYs],
        config: {
            iterations: parseInt(document.getElementById('iterations').value) || 100,
            population: parseInt(document.getElementById('population').value) || 100,
            maxLeafs: parseInt(document.getElementById('max-leafs').value) || 40,
            seed: document.getElementById('seed').value || null,
            scoringMethod: progressData['scoring-method']
        },
        allSolutions: null,
        scoreHistory: scoreHistory
    };
    jobHistory.unshift(job);
    renderJobHistory();
}

// Render job history list
function renderJobHistory() {
    const section = document.getElementById('job-history-section');
    const container = document.getElementById('job-history');

    if (jobHistory.length === 0) {
        section.classList.add('hidden');
        return;
    }

    section.classList.remove('hidden');
    container.innerHTML = jobHistory.map((job, index) => {
        const statusBadge = job.status === 'stopped'
            ? '<span class="px-1.5 py-0.5 text-xs bg-yellow-600 text-white rounded ml-2">Stopped</span>'
            : '';
        const datasetBadge = job.datasetName
            ? `<span class="px-1.5 py-0.5 text-xs bg-blue-600 text-white rounded ml-2">${job.datasetName}</span>`
            : '';
        const iterationInfo = job.status === 'stopped' && job.iteration
            ? ` · Stopped at ${job.iteration}/${job.totalIterations}`
            : '';
        const complexityInfo = job.leafCount ? `${job.leafCount} nodes` : 'N/A';

        return `
        <div class="bg-gray-800 rounded-lg overflow-hidden">
            <div class="p-4 cursor-pointer hover:bg-gray-750" onclick="toggleHistoryItem(${index})">
                <div class="flex items-center justify-between">
                    <div class="flex-1 min-w-0">
                        <div class="flex items-center space-x-2">
                            <svg id="chevron-${index}" class="w-4 h-4 text-gray-400 transform transition-transform" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                            </svg>
                            <span class="text-green-400 text-sm font-mono truncate">${job.formula}</span>
                            ${statusBadge}
                            ${datasetBadge}
                        </div>
                        <div class="text-xs text-gray-500 mt-1 ml-6">
                            Score: ${job.score.toFixed(6)} · ${job.xs.length} points${iterationInfo} · ${job.timestamp}
                        </div>
                    </div>
                    <button onclick="event.stopPropagation(); loadFromHistory(${index})" class="ml-2 px-2 py-1 text-xs bg-blue-600 hover:bg-blue-700 text-white rounded" title="Load this data">
                        Load
                    </button>
                    <button onclick="event.stopPropagation(); removeFromHistory(${index})" class="ml-1 p-1 text-gray-500 hover:text-red-400 transition-colors" title="Remove from history">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
                        </svg>
                    </button>
                </div>
            </div>
            <div id="history-details-${index}" class="hidden border-t border-gray-700">
                <div class="p-4 space-y-3">
                    <div class="grid grid-cols-2 gap-4 text-xs">
                        <div>
                            <span class="text-gray-400">Score:</span>
                            <span class="text-white ml-1">${job.score.toFixed(6)}</span>
                        </div>
                        <div>
                            <span class="text-gray-400">Complexity:</span>
                            <span class="text-white ml-1">${complexityInfo}</span>
                        </div>
                        <div>
                            <span class="text-gray-400">Iterations:</span>
                            <span class="text-white ml-1">${job.status === 'stopped' ? `${job.iteration}/${job.totalIterations}` : job.config.iterations}</span>
                        </div>
                        <div>
                            <span class="text-gray-400">Population:</span>
                            <span class="text-white ml-1">${job.config.population}</span>
                        </div>
                        <div>
                            <span class="text-gray-400">Scoring:</span>
                            <span class="text-white ml-1">${getScoringMethodDisplay(job.config.scoringMethod)}</span>
                        </div>
                        ${job.config.seed ? `<div>
                            <span class="text-gray-400">Seed:</span>
                            <span class="text-white ml-1">${job.config.seed}</span>
                        </div>` : ''}
                    </div>
                    ${job.scoreHistory && job.scoreHistory.length > 0 ? `
                    <div>
                        <div class="text-gray-400 text-xs mb-1">Score Progression:</div>
                        <div id="history-score-chart-${index}" class="bg-gray-900 rounded" style="width: 100%; height: 80px;"></div>
                    </div>
                    ` : ''}
                    <div>
                        <div class="text-gray-400 text-xs mb-1">Input Data (${job.xs.length} points):</div>
                        <div class="text-xs font-mono bg-gray-900 p-2 rounded max-h-16 overflow-auto">
                            <div><span class="text-gray-500">X:</span> <span class="text-white">${job.xs.map(x => x.toFixed(4)).join(', ')}</span></div>
                            <div><span class="text-gray-500">Y:</span> <span class="text-white">${job.ys.map(y => y.toFixed(4)).join(', ')}</span></div>
                        </div>
                    </div>
                    <div id="history-chart-${index}" class="bg-gray-900 rounded" style="width: 100%; height: 200px;"></div>
                </div>
            </div>
        </div>
    `}).join('');
}

// Toggle history item expansion
function toggleHistoryItem(index) {
    const details = document.getElementById(`history-details-${index}`);
    const chevron = document.getElementById(`chevron-${index}`);
    const isHidden = details.classList.contains('hidden');
    const job = jobHistory[index];

    if (isHidden) {
        details.classList.remove('hidden');
        chevron.classList.add('rotate-90');
        setTimeout(() => {
            renderHistoryChart(index);
            // Render score chart if history data exists
            if (job && job.scoreHistory && job.scoreHistory.length > 0) {
                renderScoreChart(`history-score-chart-${index}`, job.scoreHistory, `history-score-${index}`);
            }
        }, 50);
    } else {
        details.classList.add('hidden');
        chevron.classList.remove('rotate-90');
        if (historyCharts[index]) {
            historyCharts[index].dispose();
            delete historyCharts[index];
        }
        disposeScoreChart(`history-score-${index}`);
    }
}

// Remove job from history
function removeFromHistory(index) {
    // Dispose charts if they exist
    if (historyCharts[index]) {
        historyCharts[index].dispose();
        delete historyCharts[index];
    }
    disposeScoreChart(`history-score-${index}`);
    // Remove from array
    jobHistory.splice(index, 1);
    // Re-render (this will update all indices)
    renderJobHistory();
}

// Load data from history item
function loadFromHistory(index) {
    const job = jobHistory[index];
    if (!job) return;

    document.getElementById('xs').value = job.xs.join(', ');
    document.getElementById('ys').value = job.ys.join(', ');
    document.getElementById('iterations').value = job.config.iterations;
    document.getElementById('population').value = job.config.population;
    document.getElementById('max-leafs').value = job.config.maxLeafs;
    document.getElementById('seed').value = job.config.seed || '';

    // Restore scoring method if available
    const scoringMethodEl = document.getElementById('scoring-method');
    if (scoringMethodEl && job.config.scoringMethod) {
        scoringMethodEl.value = job.config.scoringMethod;
    }

    initDataEditorChart();

    document.getElementById('preset-select').value = '';
    document.getElementById('points-container').classList.add('hidden');

    window.scrollTo({ top: 0, behavior: 'smooth' });
}
