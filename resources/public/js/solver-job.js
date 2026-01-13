/**
 * Job control (start, stop, pause, SSE) with multi-job support
 */

// Multi-job state management
// Map: jobId -> { eventSource, isPaused, datasetName, startTime, scoreHistory, inputXs, inputYs, progress, status }
let activeJobs = new Map();
let selectedJobId = null;
let jobCounter = 0; // For naming jobs when no dataset name

// Legacy compatibility - these are now derived from activeJobs for the selected job
// Input data storage for form submissions
let inputXs = [];
let inputYs = [];

// Format seconds into human-readable time string
function formatETA(seconds) {
    if (seconds < 0 || !isFinite(seconds)) return '--';

    const hours = Math.floor(seconds / 3600);
    const minutes = Math.floor((seconds % 3600) / 60);
    const secs = Math.floor(seconds % 60);

    if (hours > 0) {
        return `${hours}h ${minutes}m`;
    } else if (minutes > 0) {
        return `${minutes}m ${secs}s`;
    } else {
        return `${secs}s`;
    }
}

// Format all scores for a solution, highlighting the primary scoring method - with sparklines
// Shows raw scores (without length deduction) for fair comparison, with deduction shown separately
function formatAllScores(solution, primaryMethod, scoreHistory) {
    const scores = solution.scores;
    const rawScores = solution.rawScores;
    const lengthDeductions = solution.lengthDeductions;

    if (!scores) {
        // Fallback for solutions without multi-score data
        return `<div class="text-sm mb-4">
            <span class="text-gray-400">Score:</span>
            <span class="text-white ml-2">${solution.score.toFixed(6)}</span>
        </div>`;
    }

    const methods = [
        { key: 'mae-max', name: 'MAE + Max' },
        { key: 'log-cosh', name: 'Log-Cosh' },
        { key: 'r-squared', name: 'R²' }
    ];

    // Use raw scores for display if available, fall back to adjusted scores
    const displayScores = rawScores || scores;

    return `<div class="grid grid-cols-3 gap-2 text-sm mb-4">
        ${methods.map(m => {
            const isPrimary = m.key === primaryMethod;
            const score = displayScores[m.key];
            const deduction = lengthDeductions ? lengthDeductions[m.key] : 0;
            const scoreStr = (score !== undefined && score !== null) ? score.toFixed(6) : '--';
            const sparkline = generateMethodSparkline(scoreHistory, m.key);
            const deductionStr = formatLengthDeduction(deduction);
            return `<div class="p-2 rounded ${isPrimary ? 'bg-blue-900/50 border border-blue-700' : 'bg-gray-800'}">
                <div class="text-gray-400 text-xs">${m.name}${isPrimary ? ' *' : ''}</div>
                <div class="${isPrimary ? 'text-blue-300 font-medium' : 'text-gray-300'}">${scoreStr}</div>
                ${deductionStr}
                ${sparkline}
            </div>`;
        }).join('')}
    </div>
    <div class="text-xs text-gray-500 mb-4">* Scoring method used for optimization</div>`;
}

// Format scores in a compact single-line format for other solutions
// Shows raw scores (without length deduction) for fair comparison
function formatCompactScores(solution, primaryMethod) {
    const scores = solution.scores;
    const rawScores = solution.rawScores;
    const lengthDeductions = solution.lengthDeductions;

    if (!scores) {
        return `<div class="text-gray-400 text-xs mt-1">Score: ${solution.score.toFixed(6)}</div>`;
    }

    const methods = [
        { key: 'mae-max', name: 'MAE' },
        { key: 'log-cosh', name: 'LC' },
        { key: 'r-squared', name: 'R²' }
    ];

    // Use raw scores for display if available
    const displayScores = rawScores || scores;

    // Check if there's any non-zero deduction for the primary method
    const primaryDeduction = lengthDeductions ? lengthDeductions[primaryMethod] : 0;
    const hasDeduction = primaryDeduction && primaryDeduction > 0;

    return `<div class="text-xs mt-1 flex flex-wrap gap-x-3 gap-y-1">
        ${methods.map(m => {
            const isPrimary = m.key === primaryMethod;
            const score = displayScores[m.key];
            const scoreStr = (score !== undefined && score !== null) ? score.toFixed(4) : '--';
            return `<span class="${isPrimary ? 'text-blue-300' : 'text-gray-400'}">
                ${m.name}: ${scoreStr}${isPrimary ? '*' : ''}
            </span>`;
        }).join('')}
        ${hasDeduction ? `<span class="text-yellow-500/70">(−${primaryDeduction.toFixed(4)} bias)</span>` : ''}
    </div>`;
}

// Format starting score comparison for jobs with a parent
// Uses raw scores (without length deduction) for fair comparison
function formatStartingScoreComparison(job, data) {
    const currentMethod = data['scoring-method'] || 'mae-max';
    // Prefer raw scores for comparison, fall back to adjusted scores
    const currentRawScores = data['best-raw-scores'] || data['best-scores'];

    // Get the current raw score for the current job's scoring method
    const currentScore = currentRawScores && currentRawScores[currentMethod] !== undefined
        ? currentRawScores[currentMethod]
        : data['best-score'];

    // Get the starting raw score for the same method from the parent job
    // Prefer raw scores for fair comparison across different simplicity bias settings
    let startScore = null;
    if (job.startingRawScores && job.startingRawScores[currentMethod] !== undefined) {
        startScore = job.startingRawScores[currentMethod];
    } else if (job.startingScores && job.startingScores[currentMethod] !== undefined) {
        // Fallback to adjusted scores if raw not available
        startScore = job.startingScores[currentMethod];
    } else if (job.startingScore !== null && job.startingScore !== undefined) {
        // Fallback to primary score if method-specific score not available
        startScore = job.startingScore;
    }

    if (startScore === null || startScore === undefined) return '';

    const diff = startScore - currentScore;
    const pctChange = startScore !== 0 ? (diff / Math.abs(startScore)) * 100 : 0;

    let changeIndicator = '';
    if (diff < 0) {
        // Score increased (improved - higher is better)
        changeIndicator = `<span class="text-green-400 ml-2">(improved ${Math.abs(pctChange).toFixed(1)}%)</span>`;
    } else if (diff > 0) {
        changeIndicator = `<span class="text-red-400 ml-2">(worsened ${Math.abs(pctChange).toFixed(1)}%)</span>`;
    }

    const methodDisplay = getScoringMethodDisplay(currentMethod);

    return `<div class="text-sm mb-2 p-2 bg-gray-800 rounded">
        <span class="text-gray-400">Starting ${methodDisplay}:</span>
        <span class="text-yellow-300 ml-1">${startScore.toFixed(6)}</span>
        <span class="text-gray-500 mx-2">→</span>
        <span class="text-gray-400">Current:</span>
        <span class="text-white ml-1">${currentScore.toFixed(6)}</span>
        ${changeIndicator}
    </div>`;
}

// Format length deduction display
function formatLengthDeduction(deduction) {
    if (!deduction || deduction === 0) return '';
    return `<div class="text-yellow-500/70 text-xs mt-1">−${deduction.toFixed(6)} bias</div>`;
}

// Format scores for progress display (during job run) - with sparklines
// Shows raw scores (without length deduction) for fair comparison, with deduction shown separately
function formatProgressScores(data, scoreHistory) {
    const scores = data['best-scores'];
    const rawScores = data['best-raw-scores'];
    const lengthDeductions = data['length-deductions'];
    const primaryMethod = data['scoring-method'] || 'mae-max';

    if (!scores) {
        // Fallback for progress without multi-score data
        return `<div class="text-sm">
            <span class="text-gray-400">Score:</span>
            <span class="text-white ml-1">${data['best-score'].toFixed(6)}</span>
        </div>`;
    }

    const methods = [
        { key: 'mae-max', name: 'MAE + Max' },
        { key: 'log-cosh', name: 'Log-Cosh' },
        { key: 'r-squared', name: 'R²' }
    ];

    // Use raw scores for display if available, fall back to adjusted scores
    const displayScores = rawScores || scores;

    return `<div class="grid grid-cols-3 gap-2 text-sm">
        ${methods.map(m => {
            const isPrimary = m.key === primaryMethod;
            const score = displayScores[m.key];
            const deduction = lengthDeductions ? lengthDeductions[m.key] : 0;
            const scoreStr = (score !== undefined && score !== null) ? score.toFixed(6) : '--';
            const sparkline = generateMethodSparkline(scoreHistory, m.key);
            const deductionStr = formatLengthDeduction(deduction);
            return `<div class="p-2 rounded ${isPrimary ? 'bg-blue-900/50 border border-blue-700' : 'bg-gray-800'}">
                <div class="text-gray-400 text-xs">${m.name}${isPrimary ? ' *' : ''}</div>
                <div class="${isPrimary ? 'text-blue-300 font-medium' : 'text-gray-300'}">${scoreStr}</div>
                ${deductionStr}
                ${sparkline}
            </div>`;
        }).join('')}
    </div>`;
}

// ============================================================================
// Tab Management
// ============================================================================

const MAX_VISIBLE_TABS = 2;

// Get ordered list of job IDs (most recent first based on startTime)
function getOrderedJobIds() {
    return Array.from(activeJobs.entries())
        .sort((a, b) => b[1].startTime - a[1].startTime)
        .map(([jobId]) => jobId);
}

// Generate short display name for a job
function getJobDisplayName(job, short = false) {
    // Strip formula portion from dataset name (e.g., "Feynman Diffraction (sin²(5x/2)...)" -> "Feynman Diffraction")
    const cleanDatasetName = job.datasetName ? job.datasetName.split(' (')[0] : null;

    if (short) {
        // Very short version for tabs: just number or abbreviated dataset
        if (cleanDatasetName) {
            // Take first 6 chars of dataset name
            const abbrev = cleanDatasetName.length > 6 ? cleanDatasetName.slice(0, 6) : cleanDatasetName;
            return `${abbrev}#${job.sequenceNum}`;
        }
        return `#${job.sequenceNum}`;
    }
    // Full label format: "Job #N" or "Job #N - DatasetName"
    if (cleanDatasetName) {
        return `Job #${job.sequenceNum} - ${cleanDatasetName}`;
    }
    return `Job #${job.sequenceNum}`;
}

// Update job label for an active job
function updateJobLabel(jobId, newLabel) {
    const job = activeJobs.get(jobId);
    if (job) {
        job.label = newLabel.trim() || getJobDisplayName(job);
        renderTabs();  // Update tab display
    }
}

// Create editable label HTML for a job
function createEditableLabelHtml(jobId, label, inputId) {
    return `
        <div class="flex items-center gap-2 mb-3">
            <span class="text-gray-400 text-sm">Label:</span>
            <input type="text"
                   id="${inputId}"
                   value="${escapeHtml(label)}"
                   class="flex-1 bg-gray-700 border border-gray-600 rounded px-2 py-1 text-sm text-white focus:border-blue-500 focus:outline-none"
                   placeholder="Enter a label for this job..."
                   onkeyup="updateJobLabel('${jobId}', this.value)"
                   onchange="updateJobLabel('${jobId}', this.value)"
            />
        </div>
    `;
}

// Escape HTML special characters
function escapeHtml(text) {
    if (!text) return '';
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// Render all tabs (called when jobs change)
function renderTabs() {
    const tabsList = document.getElementById('job-tabs-list');
    const tabsContainer = document.getElementById('job-tabs');

    // Close any open overflow menu when re-rendering
    closeTabOverflowMenu();

    if (activeJobs.size === 0) {
        tabsContainer.classList.add('hidden');
        return;
    }

    tabsContainer.classList.remove('hidden');
    tabsList.innerHTML = '';

    const orderedJobIds = getOrderedJobIds();

    // Ensure selected job is always in visible tabs (first position)
    let visibleJobIds = [];
    let overflowJobIds = [];

    if (selectedJobId && activeJobs.has(selectedJobId)) {
        // Selected job goes first
        visibleJobIds.push(selectedJobId);
        // Fill remaining visible slots with other jobs (in order)
        const otherJobs = orderedJobIds.filter(id => id !== selectedJobId);
        visibleJobIds = visibleJobIds.concat(otherJobs.slice(0, MAX_VISIBLE_TABS - 1));
        overflowJobIds = otherJobs.slice(MAX_VISIBLE_TABS - 1);
    } else {
        visibleJobIds = orderedJobIds.slice(0, MAX_VISIBLE_TABS);
        overflowJobIds = orderedJobIds.slice(MAX_VISIBLE_TABS);
    }

    // Render visible tabs
    visibleJobIds.forEach(jobId => {
        const job = activeJobs.get(jobId);
        tabsList.appendChild(createTabElement(jobId, job));
    });

    // Render overflow dropdown if needed
    if (overflowJobIds.length > 0) {
        tabsList.appendChild(createOverflowDropdown(overflowJobIds));
    }
}

// Create a single tab element
function createTabElement(jobId, job) {
    const tabDiv = document.createElement('div');
    tabDiv.id = `job-tab-${jobId}`;
    tabDiv.className = 'flex-1 px-2 py-1 text-xs rounded-t-md border-b-2 transition-colors flex items-center justify-between whitespace-nowrap cursor-pointer';

    // Apply selected styling
    if (selectedJobId === jobId) {
        tabDiv.classList.add('bg-gray-700', 'border-blue-500', 'text-white');
    } else {
        tabDiv.classList.add('bg-gray-800', 'border-transparent', 'text-gray-400', 'hover:text-gray-300');
    }

    tabDiv.onclick = (e) => {
        if (!e.target.closest('.tab-control')) {
            selectJobTab(jobId);
        }
    };

    const displayName = job.label || getJobDisplayName(job, true);
    const pauseHidden = job.isPaused ? 'hidden' : '';
    const resumeHidden = job.isPaused ? '' : 'hidden';

    tabDiv.innerHTML = `
        <span class="flex items-center space-x-1 min-w-0">
            <svg class="animate-spin h-3 w-3 text-blue-400 flex-shrink-0" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
            <span class="tab-name truncate">${escapeHtml(displayName)}</span>
        </span>
        <span class="flex items-center space-x-0.5 flex-shrink-0">
            <button class="tab-control p-0.5 rounded hover:bg-gray-600 text-yellow-400 hover:text-yellow-300" onclick="togglePauseForJob('${jobId}')" title="Pause/Resume">
                <svg class="w-3 h-3 tab-pause-icon ${pauseHidden}" fill="currentColor" viewBox="0 0 24 24">
                    <path d="M6 4h4v16H6V4zm8 0h4v16h-4V4z"/>
                </svg>
                <svg class="w-3 h-3 tab-resume-icon ${resumeHidden}" fill="currentColor" viewBox="0 0 24 24">
                    <path d="M8 5v14l11-7z"/>
                </svg>
            </button>
            <button class="tab-control p-0.5 rounded hover:bg-gray-600 text-red-400 hover:text-red-300" onclick="stopJobById('${jobId}')" title="Stop">
                <svg class="w-3 h-3" fill="currentColor" viewBox="0 0 24 24">
                    <path d="M6 6h12v12H6z"/>
                </svg>
            </button>
        </span>
    `;

    return tabDiv;
}

// Create overflow dropdown for additional tabs
function createOverflowDropdown(overflowJobIds) {
    const dropdown = document.createElement('div');
    dropdown.className = 'relative';
    dropdown.id = 'tab-overflow-dropdown';

    dropdown.innerHTML = `
        <button id="tab-overflow-btn" class="px-2 py-1 text-xs rounded-t-md border-b-2 border-transparent bg-gray-800 text-gray-400 hover:text-gray-300 flex items-center space-x-1" onclick="toggleTabOverflowMenu(event)">
            <span>+${overflowJobIds.length}</span>
            <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
            </svg>
        </button>
    `;

    // Store overflow job IDs for menu generation
    dropdown.dataset.jobIds = JSON.stringify(overflowJobIds);

    return dropdown;
}

function toggleTabOverflowMenu(event) {
    event.stopPropagation();

    let menu = document.getElementById('tab-overflow-menu');

    if (menu) {
        // Toggle existing menu
        menu.remove();
        return;
    }

    // Get the button position
    const btn = document.getElementById('tab-overflow-btn');
    const dropdown = document.getElementById('tab-overflow-dropdown');
    if (!btn || !dropdown) return;

    const rect = btn.getBoundingClientRect();
    const overflowJobIds = JSON.parse(dropdown.dataset.jobIds || '[]');

    // Create fixed-position menu
    menu = document.createElement('div');
    menu.id = 'tab-overflow-menu';
    menu.className = 'fixed bg-gray-800 border border-gray-700 rounded-md shadow-lg min-w-48';
    menu.style.cssText = `top: ${rect.bottom + 4}px; left: ${rect.left}px; z-index: 9999;`;

    menu.innerHTML = overflowJobIds.map(jobId => {
        const job = activeJobs.get(jobId);
        if (!job) return '';
        const displayName = job.label || getJobDisplayName(job);
        const pauseHidden = job.isPaused ? 'hidden' : '';
        const resumeHidden = job.isPaused ? '' : 'hidden';
        return `
            <div class="flex items-center justify-between px-3 py-2 hover:bg-gray-700 cursor-pointer border-b border-gray-700 last:border-b-0" onclick="selectJobTab('${jobId}'); closeTabOverflowMenu();">
                <div class="flex items-center space-x-2">
                    <svg class="animate-spin h-3 w-3 text-blue-400 flex-shrink-0" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                        <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                        <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                    </svg>
                    <span class="text-sm text-gray-300">${escapeHtml(displayName)}</span>
                </div>
                <span class="flex items-center space-x-1 ml-2" onclick="event.stopPropagation();">
                    <button class="tab-control p-0.5 rounded hover:bg-gray-600 text-yellow-400 hover:text-yellow-300" onclick="togglePauseForJob('${jobId}')" title="Pause/Resume">
                        <svg class="w-3 h-3 ${pauseHidden}" fill="currentColor" viewBox="0 0 24 24">
                            <path d="M6 4h4v16H6V4zm8 0h4v16h-4V4z"/>
                        </svg>
                        <svg class="w-3 h-3 ${resumeHidden}" fill="currentColor" viewBox="0 0 24 24">
                            <path d="M8 5v14l11-7z"/>
                        </svg>
                    </button>
                    <button class="tab-control p-0.5 rounded hover:bg-gray-600 text-red-400 hover:text-red-300" onclick="stopJobById('${jobId}')" title="Stop">
                        <svg class="w-3 h-3" fill="currentColor" viewBox="0 0 24 24">
                            <path d="M6 6h12v12H6z"/>
                        </svg>
                    </button>
                </span>
            </div>
        `;
    }).join('');

    document.body.appendChild(menu);
}

function closeTabOverflowMenu() {
    const menu = document.getElementById('tab-overflow-menu');
    if (menu) {
        menu.remove();
    }
}

// Close dropdown when clicking outside
document.addEventListener('click', (e) => {
    const menu = document.getElementById('tab-overflow-menu');
    const btn = document.getElementById('tab-overflow-btn');
    if (menu && btn && !menu.contains(e.target) && !btn.contains(e.target)) {
        closeTabOverflowMenu();
    }
});

function createJobTab(jobId, name) {
    // Just re-render all tabs (this handles ordering and overflow)
    renderTabs();
    selectJobTab(jobId);
}

// Toggle pause for a specific job (called from tab control)
async function togglePauseForJob(jobId) {
    const job = activeJobs.get(jobId);
    if (!job) return;

    const endpoint = job.isPaused ? 'resume' : 'pause';

    try {
        const response = await fetch('/api/jobs/' + jobId + '/' + endpoint, {
            method: 'POST'
        });
        const data = await response.json();

        if (data.error) {
            console.error('Error toggling pause:', data.error);
            return;
        }

        job.isPaused = !job.isPaused;
        updateTabPauseIcon(jobId, job.isPaused);

        // Also update main pause button if this is the selected job
        if (selectedJobId === jobId) {
            updatePauseButtonForJob(job.isPaused);
        }
    } catch (err) {
        console.error('Error toggling pause:', err);
    }
}

// Stop a specific job by ID (called from tab control)
async function stopJobById(jobId) {
    try {
        const response = await fetch('/api/jobs/' + jobId + '/stop', {
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

// Update the pause/resume icon in a tab
function updateTabPauseIcon(jobId, isPaused) {
    // Re-render tabs to update pause icon state
    renderTabs();
}

function selectJobTab(jobId) {
    if (!activeJobs.has(jobId)) return;

    selectedJobId = jobId;

    // Re-render tabs to update styling and possibly move selected to visible area
    renderTabs();

    // Update pause button state for selected job
    const job = activeJobs.get(jobId);
    if (job) {
        updatePauseButtonForJob(job.isPaused);
    }

    // Re-render content for selected job
    renderJobContent(jobId);
}

function closeJobTab(jobId) {
    // Clean up job resources
    const job = activeJobs.get(jobId);
    if (job) {
        if (job.eventSource) {
            job.eventSource.close();
        }
        activeJobs.delete(jobId);
    }

    // Re-render tabs
    renderTabs();

    // Handle selection if no more jobs or current was closed
    if (activeJobs.size === 0) {
        selectedJobId = null;
        showDefaultResults();
        hideJobControls();
    } else if (selectedJobId === jobId) {
        // Select the most recent job
        const nextJobId = getOrderedJobIds()[0];
        selectJobTab(nextJobId);
    }
}

function renderJobContent(jobId) {
    const job = activeJobs.get(jobId);
    if (!job) return;

    const resultsDiv = document.getElementById('results');

    // Dispose existing chart before re-rendering (prevents stale reference when switching tabs)
    disposeFitChart();

    if (job.progress) {
        // Show progress
        renderProgressContent(jobId, job);
    } else {
        // Waiting for first update
        const datasetBadge = job.datasetName
            ? `<span class="px-2 py-1 text-xs bg-blue-600 text-white rounded">${job.datasetName}</span>`
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
    }
}

function renderProgressContent(jobId, job) {
    const resultsDiv = document.getElementById('results');
    const data = job.progress;

    const percent = Math.round((data.iteration / data['total-iterations']) * 100);

    // Calculate elapsed time and ETA
    const elapsedMs = Date.now() - job.startTime;
    const elapsedDisplay = formatETA(elapsedMs / 1000);
    const iterationsCompleted = data.iteration;
    const iterationsRemaining = data['total-iterations'] - iterationsCompleted;
    let etaDisplay = '--';
    if (iterationsCompleted > 0) {
        const msPerIteration = elapsedMs / iterationsCompleted;
        const remainingMs = msPerIteration * iterationsRemaining;
        etaDisplay = formatETA(remainingMs / 1000);
    }

    const datasetBadge = job.datasetName
        ? `<span class="px-2 py-1 text-xs bg-blue-600 text-white rounded">${job.datasetName}</span>`
        : '';

    // Check if we need to do a full render or just update dynamic parts
    const existingDynamic = document.getElementById('progress-dynamic-content');

    if (!existingDynamic) {
        // First render - create full structure with static label section
        disposeFitChart();

        resultsDiv.innerHTML = `
            <div class="flex items-center space-x-3 mb-4">
                <svg class="animate-spin h-5 w-5 text-blue-400" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                    <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                    <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                </svg>
                <span class="text-blue-400">Solver running...</span>
                ${datasetBadge}
            </div>
            <div id="job-label-section">${createEditableLabelHtml(jobId, job.label, 'job-label-input')}</div>
            <div id="progress-dynamic-content"></div>
            <div id="formula-latex" class="mt-2 p-2 bg-gray-900 rounded-md text-sm overflow-x-auto"></div>
            <div id="fit-chart" class="mt-4" style="width: 100%; height: 300px;"></div>
        `;
    }

    // Update only the dynamic content (progress bar, formula, scores)
    const dynamicContent = document.getElementById('progress-dynamic-content');
    if (dynamicContent) {
        dynamicContent.innerHTML = `
            <div id="progress-info" class="text-sm text-gray-400">
                <div class="mb-2">
                    <div class="flex justify-between text-sm mb-1">
                        <span>Progress</span>
                        <span class="flex items-center space-x-3">
                            <span class="text-gray-400">Elapsed: <span class="text-white">${elapsedDisplay}</span></span>
                            <span class="text-gray-400">ETA: <span class="text-white">${etaDisplay}</span></span>
                            <span>${data.iteration} / ${data['total-iterations']}</span>
                        </span>
                    </div>
                    <div class="w-full bg-gray-700 rounded-full h-2">
                        <div class="bg-blue-500 h-2 rounded-full transition-all duration-300" style="width: ${percent}%"></div>
                    </div>
                </div>
                <div class="mt-4">
                    <div class="text-gray-300 text-sm mb-1">Current Best Formula:</div>
                    <div class="group flex items-center gap-2 text-green-400 text-sm">
                        <div class="flex-1 min-w-0 overflow-x-auto font-mono whitespace-nowrap pb-1" id="current-formula">${data['best-formula']}</div>
                        <button onclick="copyFormula('current-formula')" class="flex-shrink-0 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white bg-gray-800 rounded" title="Copy to clipboard">
                            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                            </svg>
                        </button>
                    </div>
                </div>
                <div class="mt-2 text-sm flex space-x-4 mb-2">
                    <div>
                        <span class="text-gray-400">Complexity:</span>
                        <span class="text-white">${data['best-formula-leaf-count']} nodes</span>
                    </div>
                    <div>
                        <span class="text-gray-400">Optimizing:</span>
                        <span class="text-white">${getScoringMethodDisplay(data['scoring-method'])}</span>
                    </div>
                    <div>
                        <span class="text-gray-400">Simplicity Bias:</span>
                        <span class="text-white">${getSimplicityBiasDisplay(data['simplicity-bias'])}</span>
                    </div>
                </div>
                ${job.startingScore !== null ? formatStartingScoreComparison(job, data) : ''}
                ${formatProgressScores(data, job.scoreHistory)}
            </div>
        `;
    }

    // Render charts and latex
    renderFitChart(data['best-formula'], false, job.inputXs, job.inputYs);
    renderLatex('formula-latex', data['best-formula']);
}

// Store last completed job for display
let lastCompletedJob = null;

function showDefaultResults() {
    const resultsDiv = document.getElementById('results');

    // If we have a last completed/stopped job, show its results instead of default message
    if (lastCompletedJob) {
        if (lastCompletedJob.status === 'stopped') {
            showStoppedJobResults(lastCompletedJob);
        } else {
            showCompletedJobResults(lastCompletedJob);
        }
        return;
    }

    resultsDiv.innerHTML = `
        <p>Enter your data and click "Find Formula" to start the solver.</p>
        <p class="mt-4 text-sm">
            The genetic algorithm will evolve mathematical expressions to find
            the best fit for your data.
        </p>
    `;
}

function showCompletedJobResults(job) {
    const resultsDiv = document.getElementById('results');
    const { data, scoreHistory, inputXs, inputYs, datasetName, label } = job;

    const datasetBadge = datasetName
        ? `<span class="px-2 py-1 text-xs bg-blue-600 text-white rounded ml-2">${datasetName}</span>`
        : '';

    const labelDisplay = label
        ? `<div class="text-gray-300 text-sm mb-3"><span class="text-gray-500">Label:</span> ${escapeHtml(label)}</div>`
        : '';

    disposeFitChart();

    resultsDiv.innerHTML = `
        <div class="mb-4">
            <div class="flex items-center space-x-2 text-green-400 mb-4">
                <svg class="h-6 w-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"/>
                </svg>
                <span class="font-semibold">Completed!</span>
                ${datasetBadge}
            </div>
            ${labelDisplay}

            <div class="mb-4">
                <div class="text-gray-300 text-sm mb-2">Best Formula Found:</div>
                <div class="group flex items-center gap-2 text-green-400 text-sm">
                    <div class="flex-1 min-w-0 overflow-x-auto font-mono whitespace-nowrap pb-1" id="best-formula">${data['best-solution'].formula}</div>
                    <button onclick="copyFormula('best-formula')" class="flex-shrink-0 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white bg-gray-800 rounded" title="Copy to clipboard">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                        </svg>
                    </button>
                </div>
                <div id="formula-latex" class="mt-2 p-2 bg-gray-900 rounded-md text-sm overflow-x-auto"></div>
            </div>

            <div class="grid grid-cols-2 gap-4 text-sm mb-4">
                <div>
                    <span class="text-gray-400">Complexity:</span>
                    <span class="text-white ml-2">${data['best-solution'].leafCount} nodes</span>
                </div>
                <div>
                    <span class="text-gray-400">Optimized for:</span>
                    <span class="text-white ml-2">${getScoringMethodDisplay(data['scoring-method'])}</span>
                </div>
                <div>
                    <span class="text-gray-400">Simplicity bias:</span>
                    <span class="text-white ml-2">${getSimplicityBiasDisplay(data['simplicity-bias'])}</span>
                </div>
            </div>
            ${formatAllScores(data['best-solution'], data['scoring-method'], scoreHistory)}

            <div id="fit-chart" style="width: 100%; height: 300px;"></div>

            <div class="mt-4">
                <div class="text-gray-300 text-sm mb-2">Other Solutions:</div>
                <div class="space-y-2 max-h-48 overflow-y-auto">
                    ${data['all-solutions'].slice(1, 6).map((sol, i) => `
                        <div class="bg-gray-700 rounded p-2 text-sm">
                            <div class="text-gray-200 font-mono">${sol.formula}</div>
                            ${formatCompactScores(sol, data['scoring-method'])}
                        </div>
                    `).join('')}
                </div>
            </div>
        </div>
    `;

    renderFitChart(data['best-solution'].formula, true, inputXs, inputYs);
    renderLatex('formula-latex', data['best-solution'].formula);
}

function showStoppedJobResults(job) {
    const resultsDiv = document.getElementById('results');
    const { progressData, scoreHistory, inputXs, inputYs, config, datasetName, label } = job;

    const datasetBadge = datasetName
        ? `<span class="px-2 py-1 text-xs bg-blue-600 text-white rounded ml-2">${datasetName}</span>`
        : '';

    const labelDisplay = label
        ? `<div class="text-gray-300 text-sm mb-3"><span class="text-gray-500">Label:</span> ${escapeHtml(label)}</div>`
        : '';

    const scoringMethod = progressData['scoring-method'] || config?.scoringMethod || 'mae-max';
    const simplicityBias = progressData['simplicity-bias'] || config?.scoringMethod || 'tiebreaker';
    const formula = progressData['best-formula'] || 'N/A';
    const leafCount = progressData['best-formula-leaf-count'] || 'N/A';
    const iteration = progressData['iteration'] || 0;
    const totalIterations = progressData['total-iterations'] || 0;

    disposeFitChart();

    // Build scores display from progressData['best-scores']
    // Include raw scores and length deductions for proper display
    let scoresHtml = '';
    const bestScores = progressData['best-scores'];
    if (bestScores) {
        // best-scores already has the right format: {'mae-max': ..., 'log-cosh': ..., 'r-squared': ...}
        const bestSolution = {
            scores: bestScores,
            rawScores: progressData['best-raw-scores'],
            lengthDeductions: progressData['length-deductions'],
            score: progressData['best-score']
        };
        scoresHtml = formatAllScores(bestSolution, scoringMethod, scoreHistory);
    } else if (progressData['best-score'] !== undefined) {
        scoresHtml = `
            <div class="mb-4">
                <span class="text-gray-400">Score (${getScoringMethodDisplay(scoringMethod)}):</span>
                <span class="text-white ml-2">${formatScore(progressData['best-score'])}</span>
            </div>
        `;
    }

    resultsDiv.innerHTML = `
        <div class="mb-4">
            <div class="flex items-center space-x-2 text-yellow-400 mb-4">
                <svg class="h-6 w-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/>
                </svg>
                <span class="font-semibold">Stopped</span>
                <span class="text-gray-400 text-sm">(iteration ${iteration}/${totalIterations})</span>
                ${datasetBadge}
            </div>
            ${labelDisplay}

            <div class="mb-4">
                <div class="text-gray-300 text-sm mb-2">Best Formula Found:</div>
                <div class="group flex items-center gap-2 text-yellow-400 text-sm">
                    <div class="flex-1 min-w-0 overflow-x-auto font-mono whitespace-nowrap pb-1" id="best-formula">${formula}</div>
                    <button onclick="copyFormula('best-formula')" class="flex-shrink-0 p-1 opacity-0 group-hover:opacity-100 transition-opacity text-gray-400 hover:text-white bg-gray-800 rounded" title="Copy to clipboard">
                        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"/>
                        </svg>
                    </button>
                </div>
                <div id="formula-latex" class="mt-2 p-2 bg-gray-900 rounded-md text-sm overflow-x-auto"></div>
            </div>

            <div class="grid grid-cols-2 gap-4 text-sm mb-4">
                <div>
                    <span class="text-gray-400">Complexity:</span>
                    <span class="text-white ml-2">${leafCount} nodes</span>
                </div>
                <div>
                    <span class="text-gray-400">Optimized for:</span>
                    <span class="text-white ml-2">${getScoringMethodDisplay(scoringMethod)}</span>
                </div>
                <div>
                    <span class="text-gray-400">Simplicity bias:</span>
                    <span class="text-white ml-2">${getSimplicityBiasDisplay(simplicityBias)}</span>
                </div>
            </div>
            ${scoresHtml}

            <div id="fit-chart" style="width: 100%; height: 300px;"></div>
        </div>
    `;

    if (formula && formula !== 'N/A') {
        renderFitChart(formula, true, inputXs, inputYs);
        renderLatex('formula-latex', formula);
    }
}

// ============================================================================
// Job Controls (Pause/Stop)
// ============================================================================

function showJobControls() {
    const jobControls = document.getElementById('job-controls');
    if (jobControls) {
        jobControls.classList.remove('hidden');
    }
}

function hideJobControls() {
    const jobControls = document.getElementById('job-controls');
    if (jobControls) {
        jobControls.classList.add('hidden');
    }
}

function updatePauseButtonForJob(isPaused) {
    const pauseBtn = document.getElementById('pause-btn');
    const pauseBtnText = document.getElementById('pause-btn-text');

    if (isPaused) {
        pauseBtnText.textContent = 'Resume';
        pauseBtn.classList.remove('bg-yellow-600', 'hover:bg-yellow-700');
        pauseBtn.classList.add('bg-green-600', 'hover:bg-green-700');
    } else {
        pauseBtnText.textContent = 'Pause';
        pauseBtn.classList.remove('bg-green-600', 'hover:bg-green-700');
        pauseBtn.classList.add('bg-yellow-600', 'hover:bg-yellow-700');
    }
}

// Toggle pause/resume for selected job
async function togglePause() {
    if (!selectedJobId) return;

    const job = activeJobs.get(selectedJobId);
    if (!job) return;

    const endpoint = job.isPaused ? 'resume' : 'pause';

    try {
        const response = await fetch('/api/jobs/' + selectedJobId + '/' + endpoint, {
            method: 'POST'
        });
        const data = await response.json();

        if (data.error) {
            console.error('Error toggling pause:', data.error);
            return;
        }

        job.isPaused = !job.isPaused;
        updatePauseButtonForJob(job.isPaused);
        updateTabPauseIcon(selectedJobId, job.isPaused);
    } catch (err) {
        console.error('Error toggling pause:', err);
    }
}

// Stop the selected job
async function stopJob() {
    if (!selectedJobId) return;

    try {
        const response = await fetch('/api/jobs/' + selectedJobId + '/stop', {
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

// ============================================================================
// CSV Upload
// ============================================================================

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

// ============================================================================
// Job Submission
// ============================================================================

function submitSolverForm(evt) {
    evt.preventDefault();

    // Get dataset name BEFORE syncing (sync clears the name)
    const datasetName = getSelectedDatasetName();

    // Ensure any chart edits are synced to textareas before reading
    // Pass true to skip clearing dataset name (we already captured it above)
    if (typeof syncEditorToTextarea === 'function' && typeof editorData !== 'undefined' && editorData.length > 0) {
        syncEditorToTextarea(true);
    }

    const xs = document.getElementById('xs').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));
    const ys = document.getElementById('ys').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));

    // Store for legacy compatibility
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

    // Add adaptive mode, quiet logs, and eval cache settings
    const adaptiveModeEl = document.getElementById('adaptive-mode');
    const quietLogsEl = document.getElementById('quiet-logs');
    const evalCacheEl = document.getElementById('eval-cache');
    config.adaptiveMode = adaptiveModeEl && adaptiveModeEl.getAttribute('aria-checked') === 'true';
    config.quietLogs = !(quietLogsEl && quietLogsEl.getAttribute('aria-checked') === 'true'); // Inverted: "Verbose Logging" toggle
    config.useEvalCache = evalCacheEl && evalCacheEl.getAttribute('aria-checked') === 'true';

    // Add scoring method
    const scoringMethodEl = document.getElementById('scoring-method');
    if (scoringMethodEl) {
        config.scoringMethod = scoringMethodEl.value;
    }

    // Add simplicity bias
    const simplicityBiasEl = document.getElementById('simplicity-bias');
    if (simplicityBiasEl) {
        config.simplicityBias = simplicityBiasEl.value;
    }

    // Add mutations blacklist if any mutations are excluded
    const blacklist = getMutationsBlacklist();
    if (blacklist.length > 0) {
        config.mutationsBlacklist = blacklist;
    }

    console.debug('Submitting job: ', config);

    startNewJob(xs, ys, config, datasetName);
}

// Start a new job (called from form submit or history buttons)
function startNewJob(xs, ys, config, datasetName, sourceJobId = null) {
    const endpoint = sourceJobId ? `/api/jobs/${sourceJobId}/continue` : '/api/solve';

    fetch(endpoint, {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({xs: xs, ys: ys, config: config})
    })
    .then(response => response.json())
    .then(data => {
        if (data.jobId) {
            // Create job entry with sequence number
            jobCounter++;

            // Find parent job's starting score if continuing from a previous job
            let startingScore = null;
            let startingScores = null;
            let startingRawScores = null;
            if (sourceJobId) {
                const parentJob = jobHistory.find(j => j.id === sourceJobId);
                if (parentJob) {
                    startingScore = parentJob.score;
                    startingScores = parentJob.scores;
                    // Store raw scores for fair comparison across different simplicity bias settings
                    startingRawScores = parentJob.rawScores || parentJob.scores;
                }
            }

            const jobEntry = {
                eventSource: null,
                isPaused: false,
                datasetName: datasetName,
                sequenceNum: jobCounter,
                startTime: Date.now(),
                scoreHistory: [],
                inputXs: xs,
                inputYs: ys,
                progress: null,
                status: 'running',
                config: config,
                label: null,  // Will be set below after we can call getJobDisplayName
                sourceJobId: sourceJobId || null,
                startingScore: startingScore,
                startingScores: startingScores,
                startingRawScores: startingRawScores
            };

            // Set default label using the display name
            jobEntry.label = getJobDisplayName(jobEntry);

            activeJobs.set(data.jobId, jobEntry);

            // Create tab and setup SSE
            createJobTab(data.jobId);
            setupSSEConnection(data.jobId);
            showJobControls();
        } else if (data.error) {
            document.getElementById('results').innerHTML = `
                <div class="text-red-400">
                    <div class="font-semibold mb-2">Error</div>
                    <div>${data.error}</div>
                </div>
            `;
        }
    })
    .catch(err => {
        document.getElementById('results').innerHTML = `
            <div class="text-red-400">
                <div class="font-semibold mb-2">Error</div>
                <div>${err.message}</div>
            </div>
        `;
    });
}

// Set up SSE connection for a specific job
function setupSSEConnection(jobId) {
    const job = activeJobs.get(jobId);
    if (!job) return;

    const eventSource = new EventSource('/api/jobs/' + jobId + '/events');
    job.eventSource = eventSource;

    eventSource.addEventListener('progress', function(e) {
        const data = JSON.parse(e.data);

        // Update job state
        job.progress = data;

        // Store all scoring method values per iteration for sparklines
        // Use raw scores (without length deduction) for consistent comparison
        const rawScores = data['best-raw-scores'] || data['best-scores'] || {};
        job.scoreHistory.push({
            iteration: data.iteration,
            score: data['best-score'],
            'mae-max': rawScores['mae-max'],
            'log-cosh': rawScores['log-cosh'],
            'r-squared': rawScores['r-squared']
        });

        // If this is the selected job, update UI
        if (selectedJobId === jobId) {
            renderProgressContent(jobId, job);
        }
    });

    eventSource.addEventListener('complete', function(e) {
        eventSource.close();
        const data = JSON.parse(e.data);

        // Capture elapsed time
        const elapsedMs = job.startTime ? Date.now() - job.startTime : null;

        // Save to history (use job.sourceJobId which we stored when starting)
        saveToHistoryMultiJob(jobId, data, 'completed', [...job.scoreHistory], job.sourceJobId, elapsedMs, job.inputXs, job.inputYs, job.config, job.datasetName, job.label);

        // Store completed job for display in Results
        lastCompletedJob = {
            status: 'completed',
            data: data,
            scoreHistory: [...job.scoreHistory],
            inputXs: job.inputXs,
            inputYs: job.inputYs,
            datasetName: job.datasetName,
            label: job.label
        };

        // Close tab (auto-close on complete)
        closeJobTab(jobId);

        // Re-enable start button if no more jobs
        updateStartButtonState();
    });

    eventSource.addEventListener('error', function(e) {
        eventSource.close();
        let errorMessage = 'Unknown error';
        if (e.data) {
            const data = JSON.parse(e.data);
            errorMessage = data.error;
        }

        // Show error if this is selected job
        if (selectedJobId === jobId) {
            document.getElementById('results').innerHTML = `
                <div class="text-red-400">
                    <div class="font-semibold mb-2">Error</div>
                    <div>${errorMessage}</div>
                </div>
            `;
        }

        // Close tab
        closeJobTab(jobId);
        updateStartButtonState();
    });

    eventSource.addEventListener('stopped', function(e) {
        eventSource.close();

        // Capture elapsed time
        const elapsedMs = job.startTime ? Date.now() - job.startTime : null;

        // Parse data and save to history (use job.sourceJobId which we stored when starting)
        if (e.data) {
            const data = JSON.parse(e.data);
            if (data['last-progress']) {
                const progressData = data['last-progress'];
                saveStoppedToHistoryMultiJob(jobId, progressData, [...job.scoreHistory], job.sourceJobId, elapsedMs, job.inputXs, job.inputYs, job.config, job.datasetName, job.label);

                // Store stopped job for display in Results
                lastCompletedJob = {
                    status: 'stopped',
                    progressData: progressData,
                    scoreHistory: [...job.scoreHistory],
                    inputXs: job.inputXs,
                    inputYs: job.inputYs,
                    config: job.config,
                    datasetName: job.datasetName,
                    label: job.label
                };
            }
        }

        // Close tab (auto-close on stop)
        closeJobTab(jobId);
        updateStartButtonState();
    });

    eventSource.onerror = function() {
        // Connection error - might be normal end of stream
    };
}

function updateStartButtonState() {
    const hasRunningJobs = activeJobs.size > 0;

    if (hasRunningJobs) {
        showJobControls();
    } else {
        hideJobControls();
    }
}

// ============================================================================
// History Integration (multi-job versions)
// ============================================================================

function saveToHistoryMultiJob(jobId, jobData, status, scoreHistory, sourceJobId, elapsedMs, xs, ys, config, datasetName, label) {
    const job = {
        id: jobId,
        parentId: sourceJobId || null,
        timestamp: new Date().toLocaleString(),
        status: status,
        datasetName: datasetName,
        label: label,
        formula: jobData['best-solution'].formula,
        score: jobData['best-solution'].score,
        scores: jobData['best-solution'].scores || null,  // All scoring method scores (with length deduction)
        rawScores: jobData['best-solution'].rawScores || null,  // Raw scores without length deduction
        lengthDeductions: jobData['best-solution'].lengthDeductions || null,  // Length deductions per method
        scoringMethod: jobData['scoring-method'] || 'mae-max',
        leafCount: jobData['best-solution'].leafCount,
        xs: [...xs],
        ys: [...ys],
        config: { ...config },
        allSolutions: jobData['all-solutions'],
        scoreHistory: scoreHistory,
        elapsedMs: elapsedMs
    };
    jobHistory.unshift(job);
    renderJobHistory();
}

function saveStoppedToHistoryMultiJob(jobId, progressData, scoreHistory, parentId, elapsedMs, xs, ys, config, datasetName, label) {
    if (!progressData) return;

    const job = {
        id: jobId,
        parentId: parentId,
        timestamp: new Date().toLocaleString(),
        status: 'stopped',
        datasetName: datasetName,
        label: label,
        formula: progressData['best-formula'],
        score: progressData['best-score'],
        scores: progressData['best-scores'] || null,  // All scoring method scores (with length deduction)
        rawScores: progressData['best-raw-scores'] || null,  // Raw scores without length deduction
        lengthDeductions: progressData['length-deductions'] || null,  // Length deductions per method
        scoringMethod: progressData['scoring-method'] || config.scoringMethod || 'mae-max',
        leafCount: progressData['best-formula-leaf-count'] || null,
        iteration: progressData['iteration'],
        totalIterations: progressData['total-iterations'],
        xs: [...xs],
        ys: [...ys],
        config: { ...config },
        allSolutions: null,
        scoreHistory: scoreHistory,
        elapsedMs: elapsedMs
    };
    jobHistory.unshift(job);
    renderJobHistory();
}

// Legacy functions kept for compatibility
function resetUI() {
    // This is now handled by closeJobTab
    if (selectedJobId) {
        closeJobTab(selectedJobId);
    }
}

function showRunningState() {
    // This is now handled by startNewJob and tab creation
    disposeFitChart();
}

function updatePauseButton() {
    if (selectedJobId) {
        const job = activeJobs.get(selectedJobId);
        if (job) {
            updatePauseButtonForJob(job.isPaused);
        }
    }
}

// Stop all running jobs when page is closed or reloaded
window.addEventListener('beforeunload', function() {
    // Use sendBeacon to ensure requests are sent even during page unload
    for (const jobId of activeJobs.keys()) {
        navigator.sendBeacon('/api/jobs/' + jobId + '/stop', '');
    }
});
