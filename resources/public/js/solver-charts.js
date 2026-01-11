/**
 * Chart-related functions for the solver
 */

// Chart instances
let fitChart = null;
let dataEditorChart = null;
let editorData = [];
let historyCharts = {};
let scoreCharts = {};

// Brush state
let currentBrushMode = 'point';
let brushSize = 3;
let brushDragging = false;
let brushStartY = null;

// Set brush mode and update UI
function setBrushMode(mode) {
    currentBrushMode = mode;

    // Update button styles
    document.querySelectorAll('.brush-mode-btn').forEach(btn => {
        btn.classList.remove('bg-blue-600', 'text-white');
        btn.classList.add('text-gray-300');
    });
    const activeBtn = document.getElementById(`brush-${mode}`);
    if (activeBtn) {
        activeBtn.classList.remove('text-gray-300');
        activeBtn.classList.add('bg-blue-600', 'text-white');
    }

    // Show/hide brush size control
    const sizeContainer = document.getElementById('brush-size-container');
    if (sizeContainer) {
        sizeContainer.classList.toggle('hidden', mode === 'point');
    }

    // Update hint text
    const hint = document.getElementById('brush-hint');
    if (hint) {
        const hints = {
            'point': '(drag points to adjust Y values)',
            'smooth': '(click and drag to smooth nearby points)',
            'bump': '(drag up/down to raise/lower nearby points)',
            'flatten': '(click and drag to flatten nearby points)'
        };
        hint.textContent = hints[mode] || '';
    }

    // Reinitialize handlers for the new mode
    if (dataEditorChart && editorData.length > 0) {
        setupDragHandlers();
    }
}

// Update brush size
function updateBrushSize(value) {
    brushSize = parseInt(value);
    const sizeValue = document.getElementById('brush-size-value');
    if (sizeValue) {
        sizeValue.textContent = value;
    }
}

// Initialize the data editor chart
function initDataEditorChart() {
    const container = document.getElementById('data-editor-chart');
    if (!container) return;

    // Dispose existing chart to ensure clean state
    if (dataEditorChart) {
        dataEditorChart.dispose();
        dataEditorChart = null;
    }

    dataEditorChart = echarts.init(container, 'dark');
    dataEditorChart.setOption({ animation: false });

    const xs = document.getElementById('xs').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));
    const ys = document.getElementById('ys').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));

    if (xs.length === 0 || ys.length === 0) {
        dataEditorChart.clear();
        return;
    }

    editorData = xs.map((x, i) => ({ x, y: ys[i] !== undefined ? ys[i] : 0, index: i }));
    updateDataEditorChart();
}

// Calculate symbol size based on number of points
function getEditorSymbolSize(numPoints) {
    if (numPoints <= 10) return 12;
    if (numPoints <= 25) return 10;
    if (numPoints <= 50) return 8;
    if (numPoints <= 100) return 6;
    return 4;
}

// Update the data editor chart display
function updateDataEditorChart() {
    if (!dataEditorChart || editorData.length === 0) return;

    const symbolSize = getEditorSymbolSize(editorData.length);

    const option = {
        backgroundColor: 'transparent',
        grid: { left: '12%', right: '5%', top: '10%', bottom: '15%' },
        xAxis: {
            type: 'value',
            axisLine: { lineStyle: { color: '#4b5563' } },
            axisLabel: { color: '#9ca3af', fontSize: 10 },
            splitLine: { lineStyle: { color: '#374151' } }
        },
        yAxis: {
            type: 'value',
            axisLine: { lineStyle: { color: '#4b5563' } },
            axisLabel: { color: '#9ca3af', fontSize: 10 },
            splitLine: { lineStyle: { color: '#374151' } }
        },
        series: [{
            type: 'scatter',
            symbolSize: symbolSize,
            data: editorData.map(d => [d.x, d.y]),
            itemStyle: { color: '#3b82f6' },
            cursor: 'ns-resize'
        }]
    };

    dataEditorChart.setOption(option);
    setupDragHandlers();
}

// Get indices of points within brush radius of a given index
function getPointsInBrushRadius(centerIdx) {
    const indices = [];
    for (let i = Math.max(0, centerIdx - brushSize); i <= Math.min(editorData.length - 1, centerIdx + brushSize); i++) {
        indices.push(i);
    }
    return indices;
}

// Calculate Gaussian weight for distance from center
function gaussianWeight(distance, sigma) {
    return Math.exp(-(distance * distance) / (2 * sigma * sigma));
}

// Apply smooth brush: weighted average with neighbors
function applySmooth(centerIdx) {
    const indices = getPointsInBrushRadius(centerIdx);
    if (indices.length < 2) return;

    const sigma = brushSize / 2;

    // Calculate smoothed values for affected points
    indices.forEach(idx => {
        let weightedSum = 0;
        let weightSum = 0;

        indices.forEach(neighborIdx => {
            const distance = Math.abs(neighborIdx - idx);
            const weight = gaussianWeight(distance, sigma);
            weightedSum += editorData[neighborIdx].y * weight;
            weightSum += weight;
        });

        // Blend original with smoothed (strength based on proximity to brush center)
        const centerDistance = Math.abs(idx - centerIdx);
        const blendFactor = gaussianWeight(centerDistance, sigma) * 0.5;
        const smoothedY = weightedSum / weightSum;
        editorData[idx].y = editorData[idx].y * (1 - blendFactor) + smoothedY * blendFactor;
    });
}

// Apply bump brush: raise or lower points based on delta Y
function applyBump(centerIdx, deltaY) {
    const indices = getPointsInBrushRadius(centerIdx);
    const sigma = brushSize / 2;

    indices.forEach(idx => {
        const distance = Math.abs(idx - centerIdx);
        const weight = gaussianWeight(distance, sigma);
        editorData[idx].y += deltaY * weight * 0.3; // Scale factor for smoother control
    });
}

// Apply flatten brush: move points toward their local average
function applyFlatten(centerIdx) {
    const indices = getPointsInBrushRadius(centerIdx);
    if (indices.length < 2) return;

    // Calculate local average
    const avg = indices.reduce((sum, idx) => sum + editorData[idx].y, 0) / indices.length;
    const sigma = brushSize / 2;

    // Move points toward average
    indices.forEach(idx => {
        const distance = Math.abs(idx - centerIdx);
        const weight = gaussianWeight(distance, sigma) * 0.3; // Blend factor
        editorData[idx].y = editorData[idx].y * (1 - weight) + avg * weight;
    });
}

// Find nearest data point index to a pixel position
function findNearestPointIndex(pixelX) {
    if (!dataEditorChart || editorData.length === 0) return -1;

    let nearestIdx = 0;
    let nearestDist = Infinity;

    try {
        editorData.forEach((d, idx) => {
            const pointPixel = dataEditorChart.convertToPixel('grid', [d.x, d.y]);
            if (pointPixel) {
                const dist = Math.abs(pointPixel[0] - pixelX);
                if (dist < nearestDist) {
                    nearestDist = dist;
                    nearestIdx = idx;
                }
            }
        });
    } catch (e) {
        return -1;
    }

    return nearestIdx;
}

// Refresh the chart display
function refreshEditorChart() {
    if (!dataEditorChart) return;
    try {
        dataEditorChart.setOption({
            series: [{ data: editorData.map(d => [d.x, d.y]) }]
        }, false);
    } catch (e) {
        console.warn('refreshEditorChart error:', e);
    }
}

// Store brush handler references so we can remove them specifically
let brushMousedownHandler = null;
let brushMousemoveHandler = null;
let brushMouseupHandler = null;
let brushGlobaloutHandler = null;

// Remove only our custom brush handlers (not all handlers)
function removeBrushHandlers() {
    if (!dataEditorChart) return;
    const zr = dataEditorChart.getZr();
    if (brushMousedownHandler) {
        zr.off('mousedown', brushMousedownHandler);
        brushMousedownHandler = null;
    }
    if (brushMousemoveHandler) {
        zr.off('mousemove', brushMousemoveHandler);
        brushMousemoveHandler = null;
    }
    if (brushMouseupHandler) {
        zr.off('mouseup', brushMouseupHandler);
        brushMouseupHandler = null;
    }
    if (brushGlobaloutHandler) {
        zr.off('globalout', brushGlobaloutHandler);
        brushGlobaloutHandler = null;
    }
}

// Set up drag handlers for points
function setupDragHandlers() {
    if (!dataEditorChart || editorData.length === 0) return;

    // Remove only our custom brush handlers (preserve ECharts internal handlers)
    removeBrushHandlers();

    const container = document.getElementById('data-editor-chart');

    if (currentBrushMode === 'point') {
        // Reset cursor for point mode
        if (container) {
            container.style.cursor = 'default';
        }

        // Point mode: handle dragging via zrender events (more reliable than graphic draggable)
        const zr = dataEditorChart.getZr();
        let draggingIdx = -1;
        let lastY = null;

        brushMousedownHandler = function(e) {
            const nearestIdx = findNearestPointIndex(e.offsetX);
            if (nearestIdx < 0) return;

            // Check if click is close enough to a point (within ~20 pixels)
            const pointPixel = dataEditorChart.convertToPixel('grid', [editorData[nearestIdx].x, editorData[nearestIdx].y]);
            const dist = Math.sqrt(Math.pow(e.offsetX - pointPixel[0], 2) + Math.pow(e.offsetY - pointPixel[1], 2));
            if (dist > 20) return;

            draggingIdx = nearestIdx;
            lastY = e.offsetY;
        };

        brushMousemoveHandler = function(e) {
            if (draggingIdx < 0) return;

            // Convert pixel Y to data Y
            const dataPos = dataEditorChart.convertFromPixel('grid', [0, e.offsetY]);
            editorData[draggingIdx].y = dataPos[1];
            lastY = e.offsetY;

            // Update chart
            dataEditorChart.setOption({
                series: [{ data: editorData.map(d => [d.x, d.y]) }]
            });
        };

        brushMouseupHandler = function(e) {
            if (draggingIdx >= 0) {
                draggingIdx = -1;
                syncEditorToTextarea();
            }
        };

        brushGlobaloutHandler = function(e) {
            if (draggingIdx >= 0) {
                draggingIdx = -1;
                syncEditorToTextarea();
            }
        };

        zr.on('mousedown', brushMousedownHandler);
        zr.on('mousemove', brushMousemoveHandler);
        zr.on('mouseup', brushMouseupHandler);
        zr.on('globalout', brushGlobaloutHandler);
    } else {
        // Brush modes: use zrender mouse events on the chart area

        // Set cursor for brush modes
        if (container) {
            container.style.cursor = currentBrushMode === 'bump' ? 'ns-resize' : 'crosshair';
        }

        const zr = dataEditorChart.getZr();
        let lastY = null;
        let lastIdx = null;

        // Define handlers as named functions so we can remove them specifically
        brushMousedownHandler = function(e) {
            // Check if click is within the grid area
            try {
                const gridModel = dataEditorChart.getModel().getComponent('grid');
                if (gridModel && gridModel.coordinateSystem) {
                    const gridRect = gridModel.coordinateSystem.getRect();
                    if (e.offsetX < gridRect.x || e.offsetX > gridRect.x + gridRect.width ||
                        e.offsetY < gridRect.y || e.offsetY > gridRect.y + gridRect.height) {
                        return;
                    }
                }
            } catch (err) {
                // If we can't get the grid rect, proceed anyway
            }

            brushDragging = true;
            brushStartY = e.offsetY;
            lastY = e.offsetY;

            const nearestIdx = findNearestPointIndex(e.offsetX);
            lastIdx = nearestIdx;

            if (nearestIdx >= 0) {
                if (currentBrushMode === 'smooth') {
                    applySmooth(nearestIdx);
                    refreshEditorChart();
                } else if (currentBrushMode === 'flatten') {
                    applyFlatten(nearestIdx);
                    refreshEditorChart();
                }
            }
        };

        brushMousemoveHandler = function(e) {
            if (!brushDragging) return;

            const nearestIdx = findNearestPointIndex(e.offsetX);
            if (nearestIdx < 0) return;

            if (currentBrushMode === 'bump') {
                if (Math.abs(e.offsetY - lastY) > 1) {
                    // Convert pixel delta to data delta (invert because screen Y is opposite to data Y)
                    const dataCenter = dataEditorChart.convertFromPixel('grid', [0, lastY]);
                    const dataNew = dataEditorChart.convertFromPixel('grid', [0, e.offsetY]);
                    const dataDelta = dataNew[1] - dataCenter[1];
                    applyBump(nearestIdx, dataDelta);
                    lastY = e.offsetY;
                    refreshEditorChart();
                }
            } else if (currentBrushMode === 'smooth') {
                if (nearestIdx !== lastIdx) {
                    applySmooth(nearestIdx);
                    lastIdx = nearestIdx;
                    refreshEditorChart();
                }
            } else if (currentBrushMode === 'flatten') {
                if (nearestIdx !== lastIdx) {
                    applyFlatten(nearestIdx);
                    lastIdx = nearestIdx;
                    refreshEditorChart();
                }
            }
        };

        brushMouseupHandler = function(e) {
            if (brushDragging) {
                brushDragging = false;
                syncEditorToTextarea();
            }
        };

        brushGlobaloutHandler = function(e) {
            if (brushDragging) {
                brushDragging = false;
                syncEditorToTextarea();
            }
        };

        // Register the handlers
        zr.on('mousedown', brushMousedownHandler);
        zr.on('mousemove', brushMousemoveHandler);
        zr.on('mouseup', brushMouseupHandler);
        zr.on('globalout', brushGlobaloutHandler);
    }
}

// Update drag handler positions - no longer needed since we use zrender events
// Kept for compatibility but does nothing now
function updateDragHandlerPositions() {
    // No-op: we no longer use graphic elements for point dragging
}

// Sync editor data to Y values textarea
// skipClearDatasetName: set to true when syncing for form submission (data wasn't manually edited)
function syncEditorToTextarea(skipClearDatasetName = false) {
    const ys = editorData.map(d => d.y.toFixed(6));
    document.getElementById('ys').value = ys.join(', ');
    // Clear dataset name only if user manually edited via drag (not during form submission sync)
    if (!skipClearDatasetName) {
        clearDatasetName();
    }
}

// Initialize or update the fit chart
// Optional customXs/customYs parameters for multi-job support
function renderFitChart(formula, animate = true, customXs = null, customYs = null) {
    const chartContainer = document.getElementById('fit-chart');
    if (!chartContainer) return;

    if (!fitChart) {
        fitChart = echarts.init(chartContainer, 'dark');
    }

    // Use custom xs/ys if provided, otherwise fall back to global inputXs/inputYs
    const xs = customXs || inputXs;
    const ys = customYs || inputYs;

    const dataPoints = xs.map((x, i) => ({ x: x, y: ys[i] }));
    dataPoints.sort((a, b) => a.x - b.x);
    const sortedXs = dataPoints.map(p => p.x);
    const sortedYs = dataPoints.map(p => p.y);

    const mathJsFormula = convertFormula(formula);

    try {
        const node = math.parse(mathJsFormula);
        const compiled = node.compile();

        const dataXMin = Math.min(...sortedXs);
        const dataXMax = Math.max(...sortedXs);
        const range = dataXMax - dataXMin;
        const extension = range * 0.15;
        const xMin = dataXMin - extension;
        const xMax = dataXMax + extension;
        const step = (xMax - xMin) / 100;
        const curveXs = [];
        const curveYs = [];

        // Calculate y-axis bounds from objective data with padding
        const dataYMin = Math.min(...sortedYs);
        const dataYMax = Math.max(...sortedYs);
        const yRange = dataYMax - dataYMin;
        const yPadding = Math.max(yRange * 0.5, Math.abs(dataYMax) * 0.1, Math.abs(dataYMin) * 0.1, 1);
        const yAxisMin = Math.floor(dataYMin - yPadding);
        const yAxisMax = Math.ceil(dataYMax + yPadding);

        // Clipping bounds with small margin so line visibly exits the chart
        const clipMax = yAxisMax + yPadding * 0.1;
        const clipMin = yAxisMin - yPadding * 0.1;

        for (let x = xMin; x <= xMax; x += step) {
            curveXs.push(x);
            try {
                const y = compiled.evaluate({ x: x });
                // Clip curve values to axis bounds
                if (!isFinite(y)) {
                    curveYs.push(null);
                } else if (y > clipMax) {
                    curveYs.push(clipMax);
                } else if (y < clipMin) {
                    curveYs.push(clipMin);
                } else {
                    curveYs.push(y);
                }
            } catch (e) {
                curveYs.push(null);
            }
        }

        const option = {
            animation: animate,
            backgroundColor: 'transparent',
            tooltip: {
                trigger: 'axis',
                backgroundColor: '#1f2937',
                borderColor: '#374151',
                textStyle: { color: '#f3f4f6' }
            },
            legend: {
                data: ['Objective Data', 'Best Function'],
                textStyle: { color: '#9ca3af' },
                top: 10
            },
            grid: { left: '3%', right: '4%', bottom: '3%', containLabel: true },
            xAxis: {
                type: 'value',
                name: 'x',
                nameTextStyle: { color: '#9ca3af' },
                axisLine: { lineStyle: { color: '#4b5563' } },
                axisLabel: { color: '#9ca3af' },
                splitLine: { lineStyle: { color: '#374151' } }
            },
            yAxis: {
                type: 'value',
                name: 'y',
                nameTextStyle: { color: '#9ca3af' },
                axisLine: { lineStyle: { color: '#4b5563' } },
                axisLabel: { color: '#9ca3af' },
                splitLine: { lineStyle: { color: '#374151' } },
                min: yAxisMin,
                max: yAxisMax
            },
            series: [
                {
                    name: 'Objective Data',
                    type: 'scatter',
                    symbol: 'circle',
                    symbolSize: 8,
                    data: sortedXs.map((x, i) => [x, sortedYs[i]]),
                    itemStyle: { color: '#3b82f6' }
                },
                {
                    name: 'Best Function',
                    type: 'line',
                    smooth: true,
                    showSymbol: false,
                    data: curveXs.map((x, i) => [x, curveYs[i]]),
                    lineStyle: { color: '#22c55e', width: 2 },
                    itemStyle: { color: '#22c55e' }
                }
            ]
        };

        fitChart.setOption(option);
    } catch (e) {
        console.error('Error evaluating formula:', e);
        chartContainer.innerHTML = '<div class="text-red-400 text-center py-8">Could not evaluate formula for charting</div>';
        fitChart = null;
    }
}

// Render chart for a history item
function renderHistoryChart(index) {
    const job = jobHistory[index];
    const container = document.getElementById(`history-chart-${index}`);
    if (!container || !job) return;

    if (historyCharts[index]) {
        historyCharts[index].dispose();
    }

    const chart = echarts.init(container, 'dark');
    historyCharts[index] = chart;

    const dataPoints = job.xs.map((x, i) => ({ x, y: job.ys[i] }));
    dataPoints.sort((a, b) => a.x - b.x);
    const sortedXs = dataPoints.map(p => p.x);
    const sortedYs = dataPoints.map(p => p.y);

    // Calculate y-axis bounds from objective data with padding
    const dataYMin = Math.min(...sortedYs);
    const dataYMax = Math.max(...sortedYs);
    const yRange = dataYMax - dataYMin;
    const yPadding = Math.max(yRange * 0.5, Math.abs(dataYMax) * 0.1, Math.abs(dataYMin) * 0.1, 1);
    const yAxisMin = Math.floor(dataYMin - yPadding);
    const yAxisMax = Math.ceil(dataYMax + yPadding);

    // Clipping bounds with small margin so line visibly exits the chart
    const clipMax = yAxisMax + yPadding * 0.1;
    const clipMin = yAxisMin - yPadding * 0.1;

    const mathJsFormula = convertFormula(job.formula);
    let curveXs = [], curveYs = [];
    try {
        const node = math.parse(mathJsFormula);
        const compiled = node.compile();
        const xMin = Math.min(...sortedXs);
        const xMax = Math.max(...sortedXs);
        const range = xMax - xMin;
        const step = range / 50;
        for (let x = xMin - range * 0.1; x <= xMax + range * 0.1; x += step) {
            curveXs.push(x);
            try {
                const y = compiled.evaluate({ x });
                // Clip curve values to axis bounds
                if (!isFinite(y)) {
                    curveYs.push(null);
                } else if (y > clipMax) {
                    curveYs.push(clipMax);
                } else if (y < clipMin) {
                    curveYs.push(clipMin);
                } else {
                    curveYs.push(y);
                }
            } catch (e) { curveYs.push(null); }
        }
    } catch (e) { console.error('Chart error:', e); }

    chart.setOption({
        animation: false,
        backgroundColor: 'transparent',
        grid: { left: '10%', right: '5%', top: '10%', bottom: '15%' },
        xAxis: { type: 'value', axisLine: { lineStyle: { color: '#4b5563' } }, axisLabel: { color: '#9ca3af', fontSize: 10 }, splitLine: { lineStyle: { color: '#374151' } } },
        yAxis: { type: 'value', min: yAxisMin, max: yAxisMax, axisLine: { lineStyle: { color: '#4b5563' } }, axisLabel: { color: '#9ca3af', fontSize: 10 }, splitLine: { lineStyle: { color: '#374151' } } },
        series: [
            { type: 'scatter', symbolSize: 6, data: sortedXs.map((x, i) => [x, sortedYs[i]]), itemStyle: { color: '#3b82f6' } },
            { type: 'line', smooth: true, showSymbol: false, data: curveXs.map((x, i) => [x, curveYs[i]]), lineStyle: { color: '#22c55e', width: 2 } }
        ]
    });
}

// Dispose fit chart
function disposeFitChart() {
    if (fitChart) {
        fitChart.dispose();
        fitChart = null;
    }
}

// Render a compact score progression chart (logarithmic Y-axis)
function renderScoreChart(containerId, scoreHistory, chartKey = null) {
    const container = document.getElementById(containerId);
    if (!container || !scoreHistory || scoreHistory.length === 0) {
        if (container) container.innerHTML = '<div class="text-gray-500 text-xs text-center py-4">Waiting for data...</div>';
        return;
    }

    // Need at least 2 points to draw a line
    if (scoreHistory.length < 2) {
        container.innerHTML = '<div class="text-gray-500 text-xs text-center py-4">Collecting data...</div>';
        return;
    }

    // Dispose existing chart if using a keyed chart
    const key = chartKey || containerId;
    if (scoreCharts[key]) {
        scoreCharts[key].dispose();
    }

    const chart = echarts.init(container, 'dark');
    scoreCharts[key] = chart;

    // Prepare data - scores should decrease (better), so we show them going down
    const data = scoreHistory.map(h => [h.iteration, h.score]);

    // Calculate if log scale is appropriate (scores span more than 2 orders of magnitude)
    const scores = scoreHistory.map(h => h.score).filter(s => s > 0);
    const minScore = Math.min(...scores);
    const maxScore = Math.max(...scores);
    const useLogScale = maxScore / minScore > 100;

    // Add some padding to Y range
    const yPadding = (maxScore - minScore) * 0.1 || maxScore * 0.1;

    const option = {
        animation: false,
        backgroundColor: 'transparent',
        grid: {
            left: 50,
            right: 10,
            top: 10,
            bottom: 25
        },
        tooltip: {
            trigger: 'axis',
            backgroundColor: '#1f2937',
            borderColor: '#374151',
            textStyle: { color: '#f3f4f6', fontSize: 11 },
            formatter: function(params) {
                const p = params[0];
                return `Iter ${p.data[0]}: ${p.data[1].toExponential(2)}`;
            }
        },
        xAxis: {
            type: 'value',
            name: 'iter',
            nameLocation: 'middle',
            nameGap: 12,
            nameTextStyle: { color: '#6b7280', fontSize: 10 },
            axisLine: { lineStyle: { color: '#374151' } },
            axisLabel: { color: '#6b7280', fontSize: 9 },
            splitLine: { show: false },
            min: 0
        },
        yAxis: {
            type: 'value',
            name: '',
            axisLine: { lineStyle: { color: '#374151' } },
            axisLabel: {
                color: '#6b7280',
                fontSize: 9,
                formatter: function(val) {
                    if (Math.abs(val) >= 1000000) return val.toExponential(0);
                    if (Math.abs(val) >= 1000) return (val/1000).toFixed(0) + 'k';
                    if (Math.abs(val) >= 1) return val.toFixed(0);
                    if (Math.abs(val) >= 0.01) return val.toFixed(2);
                    return val.toExponential(0);
                }
            },
            splitLine: { lineStyle: { color: '#374151', opacity: 0.5 } },
            min: Math.max(0, minScore - yPadding),
            max: maxScore + yPadding
        },
        series: [{
            type: 'line',
            smooth: true,
            showSymbol: scoreHistory.length < 20,
            symbolSize: 2,
            data: data,
            lineStyle: { color: '#f59e0b', width: 1 },
            itemStyle: { color: '#f59e0b' },
            areaStyle: {
                color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                    { offset: 0, color: 'rgba(245, 158, 11, 0.3)' },
                    { offset: 1, color: 'rgba(245, 158, 11, 0.05)' }
                ])
            }
        }]
    };

    chart.setOption(option);
}

// Dispose a score chart by key
function disposeScoreChart(key) {
    if (scoreCharts[key]) {
        scoreCharts[key].dispose();
        delete scoreCharts[key];
    }
}

// Generate inline SVG sparkline for a specific scoring method from score history
function generateMethodSparkline(scoreHistory, methodKey, width = 80, height = 20) {
    if (!scoreHistory || scoreHistory.length < 2) return '';

    // Extract scores for this method
    const scores = scoreHistory
        .map(h => h[methodKey])
        .filter(s => s !== undefined && s !== null && isFinite(s));

    if (scores.length < 2) return '';

    const minScore = Math.min(...scores);
    const maxScore = Math.max(...scores);
    const range = maxScore - minScore || 1;

    const points = scores.map((score, i) => {
        const x = (i / (scores.length - 1)) * width;
        const y = height - ((score - minScore) / range) * (height - 2) - 1;
        return `${x.toFixed(1)},${y.toFixed(1)}`;
    }).join(' ');

    return `<svg width="${width}" height="${height}" class="block mt-1">
        <polyline fill="none" stroke="#f59e0b" stroke-width="1.5" points="${points}"/>
    </svg>`;
}
