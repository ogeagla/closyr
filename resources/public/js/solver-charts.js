/**
 * Chart-related functions for the solver
 */

// Chart instances
let fitChart = null;
let dataEditorChart = null;
let editorData = [];
let historyCharts = {};
let scoreCharts = {};

// Initialize the data editor chart
function initDataEditorChart() {
    const container = document.getElementById('data-editor-chart');
    if (!container) return;

    if (!dataEditorChart) {
        dataEditorChart = echarts.init(container, 'dark');
        dataEditorChart.setOption({ animation: false });
        // Update drag handle positions after chart finishes rendering (e.g., after axis rescale)
        dataEditorChart.on('finished', function() {
            if (editorData.length > 0) {
                updateDragHandlerPositions();
            }
        });
    }

    const xs = document.getElementById('xs').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));
    const ys = document.getElementById('ys').value.split(',').map(s => parseFloat(s.trim())).filter(n => !isNaN(n));

    if (xs.length === 0 || ys.length === 0) {
        dataEditorChart.clear();
        return;
    }

    editorData = xs.map((x, i) => ({ x, y: ys[i] !== undefined ? ys[i] : 0, index: i }));
    updateDataEditorChart();
}

// Update the data editor chart display
function updateDataEditorChart() {
    if (!dataEditorChart || editorData.length === 0) return;

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
            symbolSize: 12,
            data: editorData.map(d => [d.x, d.y]),
            itemStyle: { color: '#3b82f6' },
            cursor: 'ns-resize'
        }]
    };

    dataEditorChart.setOption(option);
    setupDragHandlers();
}

// Set up drag handlers for points
function setupDragHandlers() {
    if (!dataEditorChart || editorData.length === 0) return;

    const graphicElements = editorData.map((d, idx) => ({
        type: 'circle',
        id: `drag-point-${idx}`,
        position: dataEditorChart.convertToPixel('grid', [d.x, d.y]),
        shape: { r: 10 },
        style: { fill: 'transparent' },
        cursor: 'ns-resize',
        draggable: 'vertical',
        z: 100,
        ondrag: function(e) {
            const pos = [this.x, this.y];
            const dataPos = dataEditorChart.convertFromPixel('grid', pos);
            editorData[idx].y = dataPos[1];
            dataEditorChart.setOption({
                series: [{ data: editorData.map(d => [d.x, d.y]) }]
            });
        },
        ondragend: function() {
            syncEditorToTextarea();
            // Re-sync graphic positions after axis may have rescaled
            updateDragHandlerPositions();
        }
    }));

    dataEditorChart.setOption({ graphic: graphicElements });
}

// Update drag handler positions without recreating them (after axis rescale)
function updateDragHandlerPositions() {
    if (!dataEditorChart || editorData.length === 0) return;

    const graphicUpdates = editorData.map((d, idx) => ({
        id: `drag-point-${idx}`,
        position: dataEditorChart.convertToPixel('grid', [d.x, d.y])
    }));

    dataEditorChart.setOption({ graphic: graphicUpdates });
}

// Sync editor data to Y values textarea
function syncEditorToTextarea() {
    const ys = editorData.map(d => d.y.toFixed(6));
    document.getElementById('ys').value = ys.join(', ');
    // Clear dataset name since user manually edited via drag
    clearDatasetName();
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
                curveYs.push(isFinite(y) ? y : null);
            } catch (e) { curveYs.push(null); }
        }
    } catch (e) { console.error('Chart error:', e); }

    chart.setOption({
        animation: false,
        backgroundColor: 'transparent',
        grid: { left: '10%', right: '5%', top: '10%', bottom: '15%' },
        xAxis: { type: 'value', axisLine: { lineStyle: { color: '#4b5563' } }, axisLabel: { color: '#9ca3af', fontSize: 10 }, splitLine: { lineStyle: { color: '#374151' } } },
        yAxis: { type: 'value', axisLine: { lineStyle: { color: '#4b5563' } }, axisLabel: { color: '#9ca3af', fontSize: 10 }, splitLine: { lineStyle: { color: '#374151' } } },
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
