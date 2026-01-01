/**
 * Mutations selection and filtering
 */

// All available mutations and selected state
let allMutations = [];
let selectedMutations = new Set();

// Load mutations from API
async function loadMutations() {
    try {
        const response = await fetch('/api/mutations');
        const data = await response.json();
        allMutations = data.mutations || [];

        // Select all by default
        selectedMutations = new Set(allMutations);

        renderMutationsList();
        updateMutationsCount();
    } catch (err) {
        console.error('Error loading mutations:', err);
        document.getElementById('mutations-count').textContent = '(error loading)';
    }
}

// Render the mutations checkboxes
function renderMutationsList() {
    const container = document.getElementById('mutations-list');
    if (!container) return;

    container.innerHTML = allMutations.map(label => `
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
    updateMutationsCount();
}

// Select all mutations
function selectAllMutations() {
    selectedMutations = new Set(allMutations);
    document.querySelectorAll('.mutation-checkbox').forEach(cb => cb.checked = true);
    updateMutationsCount();
}

// Deselect all mutations
function selectNoMutations() {
    selectedMutations.clear();
    document.querySelectorAll('.mutation-checkbox').forEach(cb => cb.checked = false);
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
