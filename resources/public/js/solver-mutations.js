/**
 * Mutations selection and filtering with group support
 */

// All available mutations and selected state
let allMutations = [];
let selectedMutations = new Set();

// Mutation groups - each mutation belongs to exactly one group
const MUTATION_GROUPS = {
    trig: {
        name: 'Trig',
        description: 'Sin, Cos, ArcSin, ArcCos operations',
        mutations: [
            '+Sin', '-Sin', '+Cos', '-Cos', '*Sin', '/Sin', '*Cos', '/Cos',
            'sin(x)', 'cos(x)', 'asin(x)', 'acos(x)',
            'Sin->Cos', 'Cos->Sin',
            'sin->cos', 'cos->sin', 'sin->asin', 'cos->acos',
            'b sin', 'b cos', 'b asin', 'b acos'
        ]
    },
    explog: {
        name: 'Exp/Log',
        description: 'Exponential and logarithm operations',
        mutations: [
            '+Log', '-Log', '+Exp', '-Exp',
            'log(x)', 'exp(x)',
            'b exp', 'b log'
        ]
    },
    poly: {
        name: 'Polynomial',
        description: 'x, x^2, sqrt operations',
        mutations: [
            '+x', '-x', '+x^2', '-x^2', '+x^1/2', '-x^1/2', '*x', '/x',
            'x^1/2', 'x^2',
            'b*b', 'b^1/2', 'b^-2', 'b^-1'
        ]
    },
    analytic: {
        name: 'Calculus',
        description: 'Derivatives',
        mutations: [
            'Derivative', 'b derivative'
        ]
    },
    arithmetic: {
        name: 'Arithmetic',
        description: 'Constant scaling and offsets',
        mutations: [
            '+1/2', '-1/2', '+1/10', '-1/10', '+1/100', '-1/100',
            '*2', '/2', '*10', '/10', '*100', '/100', '*1.1', '*0.9',
            '1/f', '*-1',
            'x+1/2', 'x-1/2', 'x/10', '10*x', '1/x', 'x/100', '100*x', '-1*x', '1.1*x', '0.9*x',
            'x+1/10', 'x-1/10', 'x+1/100', 'x-1/100',
            'c/2', 'c*2', 'c*-1', 'c/10', 'c*10', 'c+1/10', 'c-1/10', '1/c', 'c+1/100', 'c-1/100', 'c+1/2', 'c-1/2',
            'b*-1', 'b*1.1', 'b*0.9', 'b+0.1', 'b-0.1'
        ]
    },
    structure: {
        name: 'Structure',
        description: 'Operator swaps (+<->*, etc.)',
        mutations: [
            '+->*', '*->+', '^->*'
        ]
    }
};

// Track whether individual mutations view is expanded
let showIndividualMutations = false;

// Load mutations from API
async function loadMutations() {
    try {
        const response = await fetch('/api/mutations');
        const data = await response.json();
        allMutations = data.mutations || [];

        // Select all by default
        selectedMutations = new Set(allMutations);

        renderMutationsUI();
        updateMutationsCount();
    } catch (err) {
        console.error('Error loading mutations:', err);
        document.getElementById('mutations-count').textContent = '(error loading)';
    }
}

// Get group for a mutation label (returns null if not in any defined group)
function getMutationGroup(label) {
    for (const [groupId, group] of Object.entries(MUTATION_GROUPS)) {
        if (group.mutations.includes(label)) {
            return groupId;
        }
    }
    return null;
}

// Get all mutations in a group that actually exist in allMutations
function getGroupMutations(groupId) {
    const group = MUTATION_GROUPS[groupId];
    if (!group) return [];
    return group.mutations.filter(m => allMutations.includes(m));
}

// Check if all mutations in a group are selected
function isGroupFullySelected(groupId) {
    const groupMuts = getGroupMutations(groupId);
    return groupMuts.length > 0 && groupMuts.every(m => selectedMutations.has(m));
}

// Check if some (but not all) mutations in a group are selected
function isGroupPartiallySelected(groupId) {
    const groupMuts = getGroupMutations(groupId);
    const selectedCount = groupMuts.filter(m => selectedMutations.has(m)).length;
    return selectedCount > 0 && selectedCount < groupMuts.length;
}

// Toggle all mutations in a group
function toggleGroup(groupId, checked) {
    const groupMuts = getGroupMutations(groupId);
    groupMuts.forEach(m => {
        if (checked) {
            selectedMutations.add(m);
        } else {
            selectedMutations.delete(m);
        }
    });
    updateGroupCheckbox(groupId);
    updateIndividualCheckboxes();
    updateMutationsCount();
}

// Update a group checkbox based on its mutations' state
function updateGroupCheckbox(groupId) {
    const checkbox = document.querySelector(`input[data-group="${groupId}"]`);
    if (!checkbox) return;

    const fullySelected = isGroupFullySelected(groupId);
    const partiallySelected = isGroupPartiallySelected(groupId);

    checkbox.checked = fullySelected;
    checkbox.indeterminate = partiallySelected;
}

// Update all group checkboxes
function updateAllGroupCheckboxes() {
    Object.keys(MUTATION_GROUPS).forEach(updateGroupCheckbox);
}

// Update individual mutation checkboxes to match selectedMutations
function updateIndividualCheckboxes() {
    document.querySelectorAll('.mutation-checkbox').forEach(cb => {
        cb.checked = selectedMutations.has(cb.value);
    });
}

// Render the complete mutations UI (groups + individual)
function renderMutationsUI() {
    const container = document.getElementById('mutations-panel-content');
    if (!container) return;

    // Build groups HTML
    let groupsHtml = '<div class="mb-3">';
    groupsHtml += '<div class="text-xs text-gray-500 mb-2">Toggle by category:</div>';
    groupsHtml += '<div class="grid grid-cols-2 sm:grid-cols-3 gap-2">';

    for (const [groupId, group] of Object.entries(MUTATION_GROUPS)) {
        const groupMuts = getGroupMutations(groupId);
        if (groupMuts.length === 0) continue; // Skip empty groups

        const fullySelected = isGroupFullySelected(groupId);
        const count = groupMuts.length;

        groupsHtml += `
            <label class="flex items-center space-x-2 cursor-pointer bg-gray-800 hover:bg-gray-700 rounded px-2 py-1.5" title="${escapeHtml(group.description)}">
                <input type="checkbox"
                       class="group-checkbox rounded border-gray-600 bg-gray-700 text-blue-500 focus:ring-blue-500 focus:ring-offset-gray-900"
                       data-group="${groupId}"
                       ${fullySelected ? 'checked' : ''}
                       onchange="toggleGroup('${groupId}', this.checked)">
                <span class="text-gray-300 text-sm">${escapeHtml(group.name)}</span>
                <span class="text-gray-500 text-xs">(${count})</span>
            </label>
        `;
    }
    groupsHtml += '</div></div>';

    // Build individual mutations toggle
    let individualHtml = `
        <div class="border-t border-gray-700 pt-2">
            <button type="button" onclick="toggleIndividualMutations()"
                    class="flex items-center justify-between w-full text-left text-xs text-gray-400 hover:text-gray-300">
                <span>Show individual mutations</span>
                <svg id="individual-chevron" class="w-4 h-4 transform transition-transform ${showIndividualMutations ? 'rotate-180' : ''}" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"/>
                </svg>
            </button>
            <div id="individual-mutations-panel" class="${showIndividualMutations ? '' : 'hidden'} mt-2">
                <div class="flex space-x-2 mb-2">
                    <button type="button" onclick="selectAllMutations()" class="text-xs px-2 py-1 bg-gray-700 hover:bg-gray-600 text-gray-300 rounded">Select All</button>
                    <button type="button" onclick="selectNoMutations()" class="text-xs px-2 py-1 bg-gray-700 hover:bg-gray-600 text-gray-300 rounded">Select None</button>
                </div>
                <div id="mutations-list" class="max-h-48 overflow-y-auto bg-gray-900 rounded-md p-2 grid grid-cols-3 gap-1 text-xs">
                    ${renderMutationCheckboxes()}
                </div>
            </div>
        </div>
    `;

    container.innerHTML = groupsHtml + individualHtml;
    updateAllGroupCheckboxes();
}

// Render individual mutation checkboxes
function renderMutationCheckboxes() {
    return allMutations.map(label => `
        <label class="flex items-center space-x-1 cursor-pointer hover:bg-gray-800 rounded px-1">
            <input type="checkbox"
                   class="mutation-checkbox rounded border-gray-600 bg-gray-700 text-blue-500 focus:ring-blue-500 focus:ring-offset-gray-900"
                   value="${escapeHtml(label)}"
                   ${selectedMutations.has(label) ? 'checked' : ''}
                   onchange="toggleMutation('${escapeHtml(label)}', this.checked)">
            <span class="text-gray-300 truncate" title="${escapeHtml(label)}">${escapeHtml(label)}</span>
        </label>
    `).join('');
}

// Toggle individual mutations panel visibility
function toggleIndividualMutations() {
    showIndividualMutations = !showIndividualMutations;
    const panel = document.getElementById('individual-mutations-panel');
    const chevron = document.getElementById('individual-chevron');

    if (panel) {
        panel.classList.toggle('hidden', !showIndividualMutations);
    }
    if (chevron) {
        chevron.classList.toggle('rotate-180', showIndividualMutations);
    }
}

// Escape HTML to prevent XSS
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// Toggle a single mutation
function toggleMutation(label, checked) {
    if (checked) {
        selectedMutations.add(label);
    } else {
        selectedMutations.delete(label);
    }
    updateAllGroupCheckboxes();
    updateMutationsCount();
}

// Select all mutations
function selectAllMutations() {
    selectedMutations = new Set(allMutations);
    updateIndividualCheckboxes();
    updateAllGroupCheckboxes();
    updateMutationsCount();
}

// Deselect all mutations
function selectNoMutations() {
    selectedMutations.clear();
    updateIndividualCheckboxes();
    updateAllGroupCheckboxes();
    updateMutationsCount();
}

// Update the count display
function updateMutationsCount() {
    const countEl = document.getElementById('mutations-count');
    if (countEl) {
        const selected = selectedMutations.size;
        const total = allMutations.length;
        if (selected === total) {
            countEl.textContent = `(all ${total} selected)`;
        } else {
            countEl.textContent = `(${selected}/${total} selected)`;
        }
    }
}

// Toggle the mutations panel visibility
function toggleMutationsPanel() {
    const panel = document.getElementById('mutations-panel');
    const chevron = document.getElementById('mutations-chevron');

    if (panel.classList.contains('hidden')) {
        panel.classList.remove('hidden');
        chevron.classList.add('rotate-180');
    } else {
        panel.classList.add('hidden');
        chevron.classList.remove('rotate-180');
    }
}

// Get the blacklist (mutations NOT selected)
function getMutationsBlacklist() {
    if (selectedMutations.size === allMutations.length) {
        return []; // All selected, no blacklist needed
    }
    return allMutations.filter(m => !selectedMutations.has(m));
}

// Initialize on page load
document.addEventListener('DOMContentLoaded', loadMutations);
