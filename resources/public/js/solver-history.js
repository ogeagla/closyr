/**
 * Job history management
 */

let jobHistory = [];
let collapsedTrees = new Set(); // Track which job IDs have their children collapsed

// Get display name for scoring method
function getScoringMethodDisplay(method) {
    const displays = {
        'mae-max': 'MAE + Max',
        'log-cosh': 'Log-Cosh',
        'r-squared': 'R²'
    };
    return displays[method] || method || 'MAE + Max';
}

// Format milliseconds into human-readable duration
function formatDuration(ms) {
    if (!ms || ms < 0) return null;

    const seconds = Math.floor(ms / 1000);
    const hours = Math.floor(seconds / 3600);
    const minutes = Math.floor((seconds % 3600) / 60);
    const secs = seconds % 60;

    if (hours > 0) {
        return `${hours}h ${minutes}m`;
    } else if (minutes > 0) {
        return `${minutes}m ${secs}s`;
    } else {
        return `${secs}s`;
    }
}

// Save job to history
function saveToHistory(jobData, status = 'completed', scoreHistory = [], elapsedMs = null) {
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
        scoreHistory: scoreHistory,
        elapsedMs: elapsedMs
    };
    jobHistory.unshift(job);
    renderJobHistory();
}

// Save stopped job to history (from progress data)
function saveStoppedToHistory(progressData, scoreHistory = [], parentId = null, elapsedMs = null) {
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
        scoreHistory: scoreHistory,
        elapsedMs: elapsedMs
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

    // Sort by index (lower index = more recent due to unshift)
    const sortByNewest = (a, b) => a.index - b.index;

    // Sort roots by newest first
    roots.sort(sortByNewest);

    // Sort children recursively by newest first
    const sortChildren = (node) => {
        node.children.sort(sortByNewest);
        node.children.forEach(sortChildren);
    };
    roots.forEach(sortChildren);

    return roots;
}

// Render a single job item
function renderJobItem(node, depth = 0) {
    const { job, index, children } = node;
    const maxIndentLevels = 3;
    const indent = Math.min(depth, maxIndentLevels) * 24; // Cap indentation at 4 levels

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
    const durationInfo = job.elapsedMs ? ` · ${formatDuration(job.elapsedMs)}` : '';
    const complexityInfo = job.leafCount ? `${job.leafCount} nodes` : 'N/A';
    const sparkline = generateSparkline(job.scoreHistory);
    const childBadge = children.length > 0
        ? `<span class="px-1.5 py-0.5 text-xs bg-purple-600 text-white rounded ml-2">${children.length} run${children.length > 1 ? 's' : ''}</span>`
        : '';
    // Show depth indicator for nested jobs
    const depthBadge = depth > 0
        ? `<span class="px-1.5 py-0.5 text-xs bg-gray-600 text-gray-300 rounded ml-2">L${depth}</span>`
        : '';

    // Compare score with parent job (higher score = better)
    let improvementBadge = '';
    if (job.parentId) {
        const parentJob = jobHistory.find(j => j.id === job.parentId);
        if (parentJob && parentJob.score !== undefined && job.score !== undefined && job.config.scoringMethod === parentJob.config.scoringMethod) {
            const diff = job.score - parentJob.score; // positive = improved
            const pctChange = parentJob.score !== 0 ? (diff / Math.abs(parentJob.score)) * 100 : 0;
            if (diff > 0) {
                // Improved (lower score is better)
                const arrow = '↑';
                const displayPct = Math.abs(pctChange).toFixed(1);
                improvementBadge = `<span class="px-1.5 py-0.5 text-xs bg-green-700 text-green-200 rounded ml-2" title="Score improved by ${displayPct}% vs parent">${arrow}${displayPct}%</span>`;
            } else if (diff < 0) {
                // Worsened
                const arrow = '↓';
                const displayPct = Math.abs(pctChange).toFixed(1);
                improvementBadge = `<span class="px-1.5 py-0.5 text-xs bg-red-700 text-red-200 rounded ml-2" title="Score worsened by ${displayPct}% vs parent">${arrow}${displayPct}%</span>`;
            } else {
                improvementBadge = `<span class="px-1.5 py-0.5 text-xs bg-gray-700 text-gray-300 rounded ml-2" title="Same score as parent">=</span>`;
            }
        }
    }

    // Border colors for different nesting levels
    const depthBorderColors = [
        '',                      // depth 0: no border
        'border-purple-500/30',  // depth 1: purple
        'border-blue-500/30',    // depth 2: blue
        'border-teal-500/30',    // depth 3: teal
        'border-yellow-500/30',  // depth 4: yellow
        'border-amber-500/30',   // depth 5: amber
        'border-orange-500/30',  // depth 6: orange
        'border-red-500/30',     // depth 7+: red
    ];
    const borderColor = depth > 0 ? depthBorderColors[Math.min(depth, depthBorderColors.length - 1)] : '';

    // Check if this tree is collapsed
    const isTreeCollapsed = collapsedTrees.has(job.id);
    const hasChildren = children.length > 0;

    // Tree expand/collapse icon (only shown if has children)
    const treeToggleIcon = hasChildren ? `
        <button onclick="toggleTreeCollapse('${job.id}', event)" class="p-1 -ml-1 mr-1 text-gray-400 hover:text-white transition-colors flex-shrink-0" title="${isTreeCollapsed ? 'Expand children' : 'Collapse children'}">
            <svg class="w-4 h-4 transform transition-transform ${isTreeCollapsed ? '' : 'rotate-90'}" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
            </svg>
        </button>
    ` : `<span class="w-6 flex-shrink-0"></span>`; // Spacer for alignment when no children

    // Render the job
    let html = `
    <div class="bg-gray-800 rounded-lg overflow-hidden ${depth > 0 ? `border-l-2 ${borderColor}` : ''}" style="margin-left: ${indent}px;">
        <div class="p-4 cursor-pointer hover:bg-gray-750" onclick="toggleHistoryItem(${index})">
            <div class="flex items-center justify-between">
                ${treeToggleIcon}
                <div class="flex-1 min-w-0">
                    <div class="flex items-center space-x-2">
                        <svg id="chevron-${index}" class="w-4 h-4 text-gray-400 transform transition-transform flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/>
                        </svg>
                        <span class="text-green-400 text-sm font-mono truncate">${job.formula}</span>
                    </div>
                    <div class="mt-1 ml-4 flex items-center">
                        ${depthBadge}
                        ${improvementBadge}
                        ${statusBadge}
                        ${scoringBadge}
                        ${datasetBadge}
                        ${childBadge}
                    </div>
                    <div class="text-xs text-gray-500 mt-1 ml-6 flex items-center">
                        <span>Score: ${job.score.toFixed(6)} · ${job.xs.length} points${durationInfo}${iterationInfo} · ${job.timestamp}</span>
                        ${sparkline}
                    </div>
                </div>
                <button onclick="event.stopPropagation(); rerunFromHistory(${index})" class="ml-2 px-2 py-1 text-xs bg-green-600 hover:bg-green-700 text-white rounded flex-shrink-0" title="Continue evolution using job's original config">
                    Rerun
                </button>
                <button onclick="event.stopPropagation(); keepGoingFromHistory(${index})" class="ml-1 px-2 py-1 text-xs bg-purple-600 hover:bg-purple-700 text-white rounded flex-shrink-0" title="Continue evolution using current form config">
                    New Config
                </button>
                <button onclick="event.stopPropagation(); loadFromHistory(${index})" class="ml-1 px-2 py-1 text-xs bg-blue-600 hover:bg-blue-700 text-white rounded flex-shrink-0" title="Load this data">
                    Load
                </button>
                ${children.length > 0 ? `
                <button onclick="event.stopPropagation(); removeWithChildrenFromHistory(${index})" class="ml-1 p-1 text-gray-500 hover:text-red-400 transition-colors flex-shrink-0" title="Remove with all ${children.length} child run${children.length > 1 ? 's' : ''}">
                    <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"/>
                    </svg>
                </button>
                ` : ''}
                <button onclick="event.stopPropagation(); removeFromHistory(${index})" class="ml-1 p-1 text-gray-500 hover:text-red-400 transition-colors flex-shrink-0" title="Remove this item only">
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
                    <div class="group flex items-center gap-2 text-green-400 text-sm bg-gray-900 p-2 rounded">
                        <div class="flex-1 min-w-0 overflow-x-auto font-mono whitespace-nowrap" id="history-formula-${index}">${job.formula}</div>
                        <button onclick="event.stopPropagation(); copyFormula('history-formula-${index}')" class="flex-shrink-0 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white bg-gray-700 rounded" title="Copy to clipboard">
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

    // Render children (if not collapsed)
    if (!isTreeCollapsed) {
        children.forEach(child => {
            html += renderJobItem(child, depth + 1);
        });
    }

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

// Get all descendant job IDs for a given job
function getDescendantJobIds(jobId) {
    const descendants = [];
    const findChildren = (parentId) => {
        jobHistory.forEach(job => {
            if (job.parentId === parentId) {
                descendants.push(job.id);
                findChildren(job.id); // Recursively find children of children
            }
        });
    };
    findChildren(jobId);
    return descendants;
}

// Remove job and all its descendants from history
function removeWithChildrenFromHistory(index) {
    const job = jobHistory[index];
    if (!job) return;

    // Get all descendant IDs
    const descendantIds = getDescendantJobIds(job.id);
    const idsToRemove = new Set([job.id, ...descendantIds]);

    // Dispose charts for all items being removed
    jobHistory.forEach((j, idx) => {
        if (idsToRemove.has(j.id)) {
            if (historyCharts[idx]) {
                historyCharts[idx].dispose();
                delete historyCharts[idx];
            }
            disposeScoreChart(`history-score-${idx}`);
        }
    });

    // Also remove from collapsed set
    idsToRemove.forEach(id => collapsedTrees.delete(id));

    // Filter out all jobs to remove
    jobHistory = jobHistory.filter(j => !idsToRemove.has(j.id));

    // Re-render
    renderJobHistory();
}

// Toggle tree collapse state for a job
function toggleTreeCollapse(jobId, event) {
    event.stopPropagation();
    if (collapsedTrees.has(jobId)) {
        collapsedTrees.delete(jobId);
    } else {
        collapsedTrees.add(jobId);
    }
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

// Rerun - continue evolution using job's original config and data
async function rerunFromHistory(index) {
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

    // Use job's stored config (not UI form values)
    const config = {
        iterations: job.config.iterations,
        population: job.config.population,
        maxLeafs: job.config.maxLeafs,
        scoringMethod: job.config.scoringMethod,
        adaptiveMode: job.config.adaptiveMode || false,
        useEvalCache: job.config.useEvalCache || false,
        quietLogs: job.config.quietLogs !== false, // Default to true
    };

    // Include mutations blacklist if it was stored
    if (job.config.mutationsBlacklist && job.config.mutationsBlacklist.length > 0) {
        config.mutationsBlacklist = job.config.mutationsBlacklist;
    }

    // Include seed if it was stored
    if (job.config.seed) {
        config.seed = job.config.seed;
    }

    console.log('Rerunning job with job config: ', sourceJobId, config);

    try {
        // Use startNewJob which handles tab creation
        startNewJob(job.xs, job.ys, config, job.datasetName, sourceJobId);

        // Scroll to top to see progress
        window.scrollTo({ top: 0, behavior: 'smooth' });

    } catch (error) {
        console.error('Failed to start Rerun job:', error);
        alert('Failed to rerun: ' + error.message);
    }
}

// Run From New Config - continue evolution using UI form config but job's formulas/data
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

    // Build config from current form values (not job's stored config)
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

    console.log('Continuing job with new config: ', sourceJobId, config);

    try {
        // Use startNewJob which handles tab creation
        startNewJob(job.xs, job.ys, config, job.datasetName, sourceJobId);

        // Scroll to top to see progress
        window.scrollTo({ top: 0, behavior: 'smooth' });

    } catch (error) {
        console.error('Failed to start Run From New Config job:', error);
        alert('Failed to continue: ' + error.message);
    }
}
