/**
 * Chart-related functions for the solver
 */

// Chart instances
let fitChart = null;
let dataEditorChart = null;
let editorData = [];
let historyCharts = {};

// Initialize the data editor chart
function initDataEditorChart() {
    const container = document.getElementById('data-editor-chart');
    if (!container) return;

    if (!dataEditorChart) {
        dataEditorChart = echarts.init(container, 'dark');
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
        }
    }));

    dataEditorChart.setOption({ graphic: graphicElements });
}

// Sync editor data to Y values textarea
function syncEditorToTextarea() {
    const ys = editorData.map(d => d.y.toFixed(6));
    document.getElementById('ys').value = ys.join(', ');
    // Clear dataset name since user manually edited via drag
    clearDatasetName();
}

// Initialize or update the fit chart
function renderFitChart(formula, animate = true) {
    const chartContainer = document.getElementById('fit-chart');
    if (!chartContainer) return;

    if (!fitChart) {
        fitChart = echarts.init(chartContainer, 'dark');
    }

    const dataPoints = inputXs.map((x, i) => ({ x: x, y: inputYs[i] }));
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

        for (let x = xMin; x <= xMax; x += step) {
            curveXs.push(x);
            try {
                const y = compiled.evaluate({ x: x });
                curveYs.push(isFinite(y) ? y : null);
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
                splitLine: { lineStyle: { color: '#374151' } }
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
