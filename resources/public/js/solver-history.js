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
        parentId: jobData['source-job'] || null, // Track parent job for hierarchy
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
function saveStoppedToHistory(progressData, scoreHistory = [], parentId = null) {
    if (!progressData) return;

    const scoringMethodEl = document.getElementById('scoring-method');
    const job = {
        id: currentJobId,
        parentId: parentId, // Track parent job for hierarchy
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

// Generate sparkline SVG from score history
function generateSparkline(scoreHistory, width = 60, height = 16) {
    if (!scoreHistory || scoreHistory.length < 2) return '';

    const scores = scoreHistory.map(h => h.score);
    const minScore = Math.min(...scores);
    const maxScore = Math.max(...scores);
    const range = maxScore - minScore || 1;

    const points = scores.map((score, i) => {
        const x = (i / (scores.length - 1)) * width;
        const y = height - ((score - minScore) / range) * (height - 2) - 1;
        return `${x},${y}`;
    }).join(' ');

    return `<svg width="${width}" height="${height}" class="inline-block ml-2 opacity-70">
        <polyline fill="none" stroke="#4ade80" stroke-width="1.5" points="${points}"/>
    </svg>`;
}

// Build hierarchical tree from flat job array
function buildJobTree() {
    // Create a map for quick lookup
    const jobMap = new Map();
    jobHistory.forEach((job, index) => {
        jobMap.set(job.id, { job, index, children: [] });
    });

    // Build tree structure
    const roots = [];
    jobHistory.forEach((job, index) => {
        const node = jobMap.get(job.id);
        if (job.parentId && jobMap.has(job.parentId)) {
            // This job has a parent in our history - add as child
            jobMap.get(job.parentId).children.push(node);
        } else {
            // This is a root job (no parent or parent not in history)
            roots.push(node);
        }
    });

    // Sort children by timestamp (newest first within each group)
    const sortChildren = (node) => {
        node.children.sort((a, b) => {
            // Jobs are already in reverse chronological order in jobHistory
            return a.index - b.index;
        });
        node.children.forEach(sortChildren);
    };
    roots.forEach(sortChildren);

    return roots;
}

// Render a single job item
function renderJobItem(node, depth = 0) {
    const { job, index, children } = node;
    const indent = depth * 24; // Pixels of indentation per level

    const statusBadge = job.status === 'stopped'
        ? '<span class="px-1.5 py-0.5 text-xs bg-yellow-600 text-white rounded ml-2">Stopped</span>'
        : '';
    const datasetBadge = job.datasetName
        ? `<span class="px-1.5 py-0.5 text-xs bg-blue-600 text-white rounded ml-2">${job.datasetName}</span>`
        : '';
    const scoringBadge = job.config.scoringMethod
        ? `<span class="px-1.5 py-0.5 text-xs bg-blue-900 text-white rounded ml-2">${getScoringMethodDisplay(job.config.scoringMethod)}</span>`
        : '';
    const iterationInfo = job.status === 'stopped' && job.iteration
        ? ` · Stopped at ${job.iteration}/${job.totalIterations}`
        : '';
    const complexityInfo = job.leafCount ? `${job.leafCount} nodes` : 'N/A';
    const sparkline = generateSparkline(job.scoreHistory);
    const childBadge = children.length > 0
        ? `<span class="px-1.5 py-0.5 text-xs bg-purple-600 text-white rounded ml-2">${children.length} run${children.length > 1 ? 's' : ''}</span>`
        : '';

    // Render the job
    let html = `
    <div class="bg-gray-800 rounded-lg overflow-hidden ${depth > 0 ? 'border-l-2 border-purple-500/30' : ''}" style="margin-left: ${indent}px;">
        <div class="p-4 cursor-pointer hover:bg-gray-750" onclick="toggleHistoryItem(${index})">
            <div class="flex items-center justify-between">
                <div class="flex-1 min-w-0">
                    <div class="flex items-center space-x-2">
                        <svg id="chevron-${index}" class="w-4 h-4 text-gray-400 transform transition-transform flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                        </svg>
                        <span class="text-green-400 text-sm font-mono truncate">${job.formula}</span>
                        ${statusBadge}
                        ${scoringBadge}
                        ${datasetBadge}
                        ${childBadge}
                    </div>
                    <div class="text-xs text-gray-500 mt-1 ml-6 flex items-center">
                        <span>Score: ${job.score.toFixed(6)} · ${job.xs.length} points${iterationInfo} · ${job.timestamp}</span>
                        ${sparkline}
                    </div>
                </div>
                <button onclick="event.stopPropagation(); keepGoingFromHistory(${index})" class="ml-2 px-2 py-1 text-xs bg-green-600 hover:bg-green-700 text-white rounded flex-shrink-0" title="Continue evolution from these results">
                    Keep Going
                </button>
                <button onclick="event.stopPropagation(); loadFromHistory(${index})" class="ml-1 px-2 py-1 text-xs bg-blue-600 hover:bg-blue-700 text-white rounded flex-shrink-0" title="Load this data">
                    Load
                </button>
                <button onclick="event.stopPropagation(); removeFromHistory(${index})" class="ml-1 p-1 text-gray-500 hover:text-red-400 transition-colors flex-shrink-0" title="Remove from history">
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
                    ${job.config.seed ? `<div>
                        <span class="text-gray-400">Seed:</span>
                        <span class="text-white ml-1">${job.config.seed}</span>
                    </div>` : ''}
                </div>
                <!-- Formula with copy button -->
                <div>
                    <div class="text-gray-400 text-xs mb-1">Formula:</div>
                    <div class="group relative formula-display text-green-400 text-sm bg-gray-900 p-2 rounded">
                        <span id="history-formula-${index}">${job.formula}</span>
                        <button onclick="event.stopPropagation(); copyFormula('history-formula-${index}')" class="absolute right-1 top-1 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white" title="Copy to clipboard">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                            </svg>
                        </button>
                    </div>
                </div>
                <!-- LaTeX formula (renderLatex adds its own copy button) -->
                <div>
                    <div class="text-gray-400 text-xs mb-1">LaTeX:</div>
                    <div id="history-latex-${index}" class="bg-gray-900 p-2 rounded overflow-x-auto text-sm"></div>
                </div>
                <div>
                    <div class="text-gray-400 text-xs mb-1">Input Data (${job.xs.length} points):</div>
                    <div class="text-xs font-mono bg-gray-900 p-2 rounded max-h-16 overflow-auto">
                        <div><span class="text-gray-500">X:</span> <span class="text-white">${job.xs.map(x => x.toFixed(4)).join(', ')}</span></div>
                        <div><span class="text-gray-500">Y:</span> <span class="text-white">${job.ys.map(y => y.toFixed(4)).join(', ')}</span></div>
                    </div>
                </div>
                <!-- Collapsible charts section -->
                <div class="border border-gray-700 rounded-lg overflow-hidden">
                    <div class="bg-gray-750 p-2 cursor-pointer flex items-center justify-between" onclick="event.stopPropagation(); toggleHistoryCharts(${index})">
                        <span class="text-gray-400 text-xs font-medium">Charts & Visualizations</span>
                        <svg id="charts-chevron-${index}" class="w-4 h-4 text-gray-400 transform transition-transform" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                        </svg>
                    </div>
                    <div id="history-charts-${index}" class="hidden p-3 space-y-3 bg-gray-900">
                        ${job.scoreHistory && job.scoreHistory.length > 0 ? `
                        <div>
                            <div class="text-gray-400 text-xs mb-1">Score Progression:</div>
                            <div id="history-score-chart-${index}" class="bg-gray-800 rounded" style="width: 100%; height: 80px;"></div>
                        </div>
                        ` : ''}
                        <div>
                            <div class="text-gray-400 text-xs mb-1">Best Formula Fit:</div>
                            <div id="history-chart-${index}" class="bg-gray-800 rounded" style="width: 100%; height: 200px;"></div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>`;

    // Render children
    children.forEach(child => {
        html += renderJobItem(child, depth + 1);
    });

    return html;
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

    // Build hierarchical tree and render
    const tree = buildJobTree();
    container.innerHTML = tree.map(node => renderJobItem(node, 0)).join('');
}

// Toggle charts visibility within a history item
function toggleHistoryCharts(index) {
    const chartsContainer = document.getElementById(`history-charts-${index}`);
    const chartsChevron = document.getElementById(`charts-chevron-${index}`);
    const job = jobHistory[index];

    if (!chartsContainer) return;

    const isHidden = chartsContainer.classList.contains('hidden');

    if (isHidden) {
        chartsContainer.classList.remove('hidden');
        chartsChevron.classList.add('rotate-90');
        // Render charts after showing container
        // Use requestAnimationFrame to ensure browser has completed layout
        requestAnimationFrame(() => {
            requestAnimationFrame(() => {
                renderHistoryChart(index);
                // Force resize to ensure proper rendering
                if (historyCharts[index]) {
                    historyCharts[index].resize();
                }
                if (job && job.scoreHistory && job.scoreHistory.length > 0) {
                    renderScoreChart(`history-score-chart-${index}`, job.scoreHistory, `history-score-${index}`);
                }
            });
        });
    } else {
        chartsContainer.classList.add('hidden');
        chartsChevron.classList.remove('rotate-90');
        // Dispose charts
        if (historyCharts[index]) {
            historyCharts[index].dispose();
            delete historyCharts[index];
        }
        disposeScoreChart(`history-score-${index}`);
    }
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
        // Render LaTeX formula when expanded
        if (job && job.formula) {
            renderLatex(`history-latex-${index}`, job.formula);
        }
    } else {
        details.classList.add('hidden');
        chevron.classList.remove('rotate-90');
        // Also collapse charts section and dispose charts
        const chartsContainer = document.getElementById(`history-charts-${index}`);
        const chartsChevron = document.getElementById(`charts-chevron-${index}`);
        if (chartsContainer) {
            chartsContainer.classList.add('hidden');
            if (chartsChevron) chartsChevron.classList.remove('rotate-90');
        }
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

// Keep Going - continue evolution from a history item's results
async function keepGoingFromHistory(index) {
    const job = jobHistory[index];
    if (!job) return;

    // Get formulas from the job
    let seedFormulas = [];
    if (job.allSolutions && job.allSolutions.length > 0) {
        seedFormulas = job.allSolutions.map(s => s.formula);
    } else if (job.formula) {
        seedFormulas = [job.formula];
    }

    if (seedFormulas.length === 0) {
        alert('No formulas available to seed from this job');
        return;
    }

    // Use the job's original job ID to call continue endpoint
    const sourceJobId = job.id;

    // Build config from current form values

    // Add adaptive mode, quiet logs, and eval cache settings
    const adaptiveModeEl = document.getElementById('adaptive-mode');
    const quietLogsEl = document.getElementById('quiet-logs');
    const evalCacheEl = document.getElementById('eval-cache');

    const config = {
        iterations: parseInt(document.getElementById('iterations').value) || job.config.iterations,
        population: parseInt(document.getElementById('population').value) || job.config.population,
        maxLeafs: parseInt(document.getElementById('max-leafs').value) || job.config.maxLeafs,
        scoringMethod: document.getElementById('scoring-method')?.value || job.config.scoringMethod,
        adaptiveMode: adaptiveModeEl && adaptiveModeEl.getAttribute('aria-checked') === 'true',
        useEvalCache: evalCacheEl && evalCacheEl.getAttribute('aria-checked') === 'true',
        quietLogs: !(quietLogsEl && quietLogsEl.getAttribute('aria-checked') === 'true'),
    };

    // Add mutations blacklist if any mutations are excluded
    const blacklist = getMutationsBlacklist();
    if (blacklist.length > 0) {
        config.mutationsBlacklist = blacklist;
    }

    // Add seed if specified
    const seedInput = document.getElementById('seed').value;
    if (seedInput) {
        config.seed = parseInt(seedInput);
    }

    console.log('Continuing job: ', sourceJobId, config);

    try {
        const response = await fetch(`/api/jobs/${sourceJobId}/continue`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                xs: job.xs,
                ys: job.ys,
                config: config
            })
        });

        if (!response.ok) {
            const error = await response.json();
            throw new Error(error.error || 'Failed to continue job');
        }

        const result = await response.json();
        currentJobId = result.jobId;

        // Show running state and connect to SSE
        showRunningState();
        setupSSEConnection(currentJobId);

        // Scroll to top to see progress
        window.scrollTo({ top: 0, behavior: 'smooth' });

    } catch (error) {
        console.error('Failed to start Keep Going job:', error);
        alert('Failed to continue: ' + error.message);
    }
}
