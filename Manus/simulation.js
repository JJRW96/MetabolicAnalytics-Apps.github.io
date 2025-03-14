// Constants for the step test protocol
const STEP_DURATION_SECONDS = 30;
const STARTING_POWER_WATTS = 60;
const POWER_INCREMENT_WATTS = 20;
const MAX_POWER_WATTS = 500;

// Default parameter values
const DEFAULT_VO2MAX_ML_MIN = 6000;
const DEFAULT_VLAMAX = 0.70;

// Conversion factors and other constants from the Mader model
const M2 = 1.66e9; // CK equilibrium constant (mol^-1)
const M3 = 0.95; // Adenylate kinase equilibrium constant
const DG0 = -30.6; // Standard free energy of the [ATP]/[PCr] system (kJ/mol)
const R = 8.314 / 1000; // Gas constant (kJ/mol/K)
const T = 310; // Temperature in Kelvin (37°C)
const DBUFF = 25; // Buffering capacity of skeletal muscle (mmol/l/pH unit)
const B_VO2 = 4.3; // Transformation coefficient of VO2 into vGP (ml O2/mmol GP)
const B_V_LA = 1.4; // Transformation coefficient of vla into vGP,vla (mmol lactate/mmol GP)
const B_POW = 0.00267; // Rate of GP consumption per watt (mmol/kg/s/watt)

// Current simulation state
let simulationState = {
    running: false,
    currentTime: 0,
    currentPower: 0,
    currentStep: 0,
    timePoints: [],
    powerPoints: []
};

// Model parameters (can be adjusted by sliders)
let modelParameters = {
    vo2max: DEFAULT_VO2MAX_ML_MIN,
    vlamax: DEFAULT_VLAMAX
};

// Simulation results storage
let simulationResults = {
    time: [],
    power: [],
    vo2: [],
    vlamax: [],
    vlaox: [],
    pyruvateDeficit: [],
    muscleLactate: [],
    bloodLactate: [],
    pH: [],
    pcr: [],
    atp: [],
    adp: []
};

// Initial values for metabolic parameters
const initialValues = {
    atp: 7.0, // mmol/kg
    pcr: 25.0, // mmol/kg
    pi: 3.0, // mmol/kg
    pH: 7.4,
    muscleLactate: 1.0, // mmol/l
    bloodLactate: 1.0, // mmol/l
    vo2: 300 // ml/min (resting)
};

// Charts
let powerChart;
let metabolicChart;

// DOM elements
let vo2maxSlider, vo2maxValue;
let vlamaxSlider, vlamaxValue;
let startBtn, stopBtn, resetBtn;
let currentTimeDisplay, currentPowerDisplay;
let currentVO2Display, currentVLAoxDisplay, currentPyruvateDeficitDisplay;
let currentMuscleLactateDisplay, currentBloodLactateDisplay, currentPHDisplay;
let currentPCrDisplay, currentATPDisplay;

// Initialize the application
document.addEventListener('DOMContentLoaded', () => {
    // Get DOM elements
    vo2maxSlider = document.getElementById('vo2max');
    vo2maxValue = document.getElementById('vo2max-value');
    vlamaxSlider = document.getElementById('vlamax');
    vlamaxValue = document.getElementById('vlamax-value');
    
    startBtn = document.getElementById('start-btn');
    stopBtn = document.getElementById('stop-btn');
    resetBtn = document.getElementById('reset-btn');
    
    currentTimeDisplay = document.getElementById('current-time');
    currentPowerDisplay = document.getElementById('current-power');
    currentVO2Display = document.getElementById('current-vo2');
    currentVLAoxDisplay = document.getElementById('current-vlaox');
    currentPyruvateDeficitDisplay = document.getElementById('current-pyruvate-deficit');
    currentMuscleLactateDisplay = document.getElementById('current-muscle-lactate');
    currentBloodLactateDisplay = document.getElementById('current-blood-lactate');
    currentPHDisplay = document.getElementById('current-ph');
    currentPCrDisplay = document.getElementById('current-pcr');
    currentATPDisplay = document.getElementById('current-atp');
    
    // Set up event listeners
    vo2maxSlider.addEventListener('input', () => {
        modelParameters.vo2max = parseInt(vo2maxSlider.value);
        vo2maxValue.textContent = modelParameters.vo2max;
    });
    
    vlamaxSlider.addEventListener('input', () => {
        modelParameters.vlamax = parseFloat(vlamaxSlider.value);
        vlamaxValue.textContent = modelParameters.vlamax.toFixed(2);
    });
    
    startBtn.addEventListener('click', startSimulation);
    stopBtn.addEventListener('click', stopSimulation);
    resetBtn.addEventListener('click', resetSimulation);
    
    // Initialize charts
    initializeCharts();
    
    // Initialize simulation
    resetSimulation();
});

// Initialize charts
function initializeCharts() {
    // Power chart
    const powerCtx = document.getElementById('power-chart').getContext('2d');
    powerChart = new Chart(powerCtx, {
        type: 'line',
        data: {
            labels: [],
            datasets: [{
                label: 'Power (watts)',
                data: [],
                borderColor: '#3498db',
                backgroundColor: 'rgba(52, 152, 219, 0.1)',
                borderWidth: 2,
                fill: true
            }]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            scales: {
                x: {
                    title: {
                        display: true,
                        text: 'Time (seconds)'
                    }
                },
                y: {
                    title: {
                        display: true,
                        text: 'Power (watts)'
                    },
                    min: 0,
                    max: MAX_POWER_WATTS + 50
                }
            }
        }
    });
    
    // Metabolic parameters chart
    const metabolicCtx = document.getElementById('metabolic-chart').getContext('2d');
    metabolicChart = new Chart(metabolicCtx, {
        type: 'line',
        data: {
            labels: [],
            datasets: [
                {
                    label: 'VO2 (ml/min)',
                    data: [],
                    borderColor: '#2ecc71',
                    backgroundColor: 'transparent',
                    borderWidth: 2,
                    yAxisID: 'y-vo2'
                },
                {
                    label: 'Muscle Lactate (mmol/l)',
                    data: [],
                    borderColor: '#e74c3c',
                    backgroundColor: 'transparent',
                    borderWidth: 2,
                    yAxisID: 'y-lactate'
                },
                {
                    label: 'Blood Lactate (mmol/l)',
                    data: [],
                    borderColor: '#9b59b6',
                    backgroundColor: 'transparent',
                    borderWidth: 2,
                    yAxisID: 'y-lactate'
                },
                {
                    label: 'pH',
                    data: [],
                    borderColor: '#f39c12',
                    backgroundColor: 'transparent',
                    borderWidth: 2,
                    yAxisID: 'y-ph'
                },
                {
                    label: 'PCr (mmol/kg)',
                    data: [],
                    borderColor: '#1abc9c',
                    backgroundColor: 'transparent',
                    borderWidth: 2,
                    yAxisID: 'y-pcr-atp'
                },
                {
                    label: 'ATP (mmol/kg)',
                    data: [],
                    borderColor: '#34495e',
                    backgroundColor: 'transparent',
                    borderWidth: 2,
                    yAxisID: 'y-pcr-atp'
                }
            ]
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            scales: {
                x: {
                    title: {
                        display: true,
                        text: 'Time (seconds)'
                    }
                },
                'y-vo2': {
                    type: 'linear',
                    position: 'left',
                    title: {
                        display: true,
                        text: 'VO2 (ml/min)'
                    },
                    min: 0,
                    max: modelParameters.vo2max * 1.1
                },
                'y-lactate': {
                    type: 'linear',
                    position: 'right',
                    title: {
                        display: true,
                        text: 'Lactate (mmol/l)'
                    },
                    min: 0,
                    max: 20,
                    grid: {
                        drawOnChartArea: false
                    }
                },
                'y-ph': {
                    type: 'linear',
                    position: 'right',
                    title: {
                        display: true,
                        text: 'pH'
                    },
                    min: 6.8,
                    max: 7.5,
                    grid: {
                        drawOnChartArea: false
                    }
                },
                'y-pcr-atp': {
                    type: 'linear',
                    position: 'right',
                    title: {
                        display: true,
                        text: 'PCr/ATP (mmol/kg)'
                    },
                    min: 0,
                    max: 30,
                    grid: {
                        drawOnChartArea: false
                    }
                }
            }
        }
    });
}

// Start the simulation
function startSimulation() {
    if (simulationState.running) return;
    
    simulationState.running = true;
    startBtn.disabled = true;
    stopBtn.disabled = false;
    
    // Start the simulation loop
    requestAnimationFrame(simulationLoop);
}

// Stop the simulation
function stopSimulation() {
    simulationState.running = false;
    startBtn.disabled = false;
    stopBtn.disabled = true;
}

// Reset the simulation
function resetSimulation() {
    stopSimulation();
    
    // Reset simulation state
    simulationState = {
        running: false,
        currentTime: 0,
        currentPower: STARTING_POWER_WATTS,
        currentStep: 0,
        timePoints: [0],
        powerPoints: [STARTING_POWER_WATTS]
    };
    
    // Reset simulation results
    simulationResults = {
        time: [0],
        power: [STARTING_POWER_WATTS],
        vo2: [initialValues.vo2],
        vlamax: [modelParameters.vlamax],
        vlaox: [0],
        pyruvateDeficit: [0],
        muscleLactate: [initialValues.muscleLactate],
        bloodLactate: [initialValues.bloodLactate],
        pH: [initialValues.pH],
        pcr: [initialValues.pcr],
        atp: [initialValues.atp],
        adp: [calculateADP(initialValues.atp, initialValues.pi, Math.pow(10, -initialValues.pH), initialValues.pcr)]
    };
    
    // Update UI
    updateUI();
    
    // Update charts
    updateCharts();
}

// Main simulation loop
function simulationLoop() {
    if (!simulationState.running) return;
    
    // Increment time (1 second per frame)
    simulationState.currentTime += 1;
    
    // Check if we need to increase power (new step)
    if (simulationState.currentTime % STEP_DURATION_SECONDS === 0) {
        simulationState.currentStep++;
        simulationState.currentPower = STARTING_POWER_WATTS + 
            (simulationState.currentStep * POWER_INCREMENT_WATTS);
        
        // Check if we've reached max power
        if (simulationState.currentPower > MAX_POWER_WATTS) {
            stopSimulation();
            return;
        }
    }
    
    // Store time and power points
    simulationState.timePoints.push(simulationState.currentTime);
    simulationState.powerPoints.push(simulationState.currentPower);
    
    // Calculate metabolic parameters for this time point
    calculateMetabolicParameters();
    
    // Update UI
    updateUI();
    
    // Update charts every 5 seconds to improve performance
    if (simulationState.currentTime % 5 === 0) {
        updateCharts();
    }
    
    // Continue simulation loop
    setTimeout(() => {
        if (simulationState.running) {
            requestAnimationFrame(simulationLoop);
        }
    }, 50); // Slow down simulation for better visualization (20 fps)
}

// Calculate ADP concentration
function calculateADP(atp, pi, hPlus, pcr) {
    // Implementation of equation: [ADP] = [ATP] * [Pi] / (M2 * [H+] * [PCr])
    return (atp * pi) / (M2 * hPlus * pcr);
}

// Calculate metabolic parameters based on the Mader model
function calculateMetabolicParameters() {
    // Get the last values
    const lastIndex = simulationResults.time.length - 1;
    const lastTime = simulationResults.time[lastIndex];
    const lastPower = simulationResults.power[lastIndex];
    const lastVO2 = simulationResults.vo2[lastIndex];
    const lastMuscleLactate = simulationResults.muscleLactate[lastIndex];
    const lastBloodLactate = simulationResults.bloodLactate[lastIndex];
    const lastPH = simulationResults.pH[lastIndex];
    const lastPCr = simulationResults.pcr[lastIndex];
    const lastATP = simulationResults.atp[lastIndex];
    const lastADP = simulationResults.adp[lastIndex];
    
    // Current power
    const currentPower = simulationState.currentPower;
    
    // Calculate ATP consumption rate based on power
    const atpConsumptionRate = B_POW * currentPower;
    
    // Calculate VO2 (simplified model with exponential rise)
    const vo2ss = Math.min(modelParameters.vo2max, 300 + (B_VO2 * atpConsumptionRate * 1000)); // Steady state VO2
    const tVO2 = 30; // Time constant for VO2 kinetics (seconds)
    const deltaVO2 = (vo2ss - lastVO2) * (1 - Math.exp(-1/tVO2));
    const currentVO2 = lastVO2 + deltaVO2;
    
    // Calculate oxidative phosphorylation rate
    const oxidativePhosphorylationRate = currentVO2 / (B_VO2 * 1000); // Convert from ml/min to mmol/s/kg
    
    // Calculate glycolysis rate (VLamax)
    const currentVLAmax = modelParameters.vlamax;
    
    // Calculate pH effect on glycolysis
    const pHEffect = Math.max(0, 1 - Math.pow((7.4 - lastPH) / 0.6, 2));
    
    // Calculate actual glycolysis rate
    const glycolysisActivation = Math.min(1, Math.pow(lastADP, 2) / (Math.pow(0.01, 2) + Math.pow(lastADP, 2)));
    const actualGlycolysisRate = currentVLAmax * glycolysisActivation * pHEffect;
    
    // Calculate lactate production and oxidation
    const lactateProdRate = actualGlycolysisRate;
    const lactateOxidationRate = Math.min(lactateProdRate, 0.1 * (lastMuscleLactate / (5 + lastMuscleLactate)));
    const netLactateProductionRate = lactateProdRate - lactateOxidationRate;
    
    // Calculate muscle lactate accumulation
    const deltaMuscleLactate = netLactateProductionRate - 0.05 * (lastMuscleLactate - lastBloodLactate);
    const currentMuscleLactate = Math.max(0, lastMuscleLactate + deltaMuscleLactate);
    
    // Calculate blood lactate accumulation
    const deltaBloodLactate = 0.02 * (lastMuscleLactate - lastBloodLactate) - 0.01 * lastBloodLactate;
    const currentBloodLactate = Math.max(0, lastBloodLactate + deltaBloodLactate);
    
    // Calculate pH
    const deltaPH = -0.005 * deltaMuscleLactate / DBUFF;
    const currentPH = Math.max(6.8, Math.min(7.4, lastPH + deltaPH));
    
    // Calculate PCr and ATP
    const totalPhosphorylationRate = oxidativePhosphorylationRate + actualGlycolysisRate;
    const phosphorylationDeficit = atpConsumptionRate - totalPhosphorylationRate;
    
    // PCr decreases to compensate for ATP deficit
    const deltaPCr = -phosphorylationDeficit;
    const currentPCr = Math.max(5, Math.min(initialValues.pcr, lastPCr + deltaPCr));
    
    // ATP is maintained relatively constant
    const deltaATP = Math.max(-0.1, Math.min(0.1, totalPhosphorylationRate - atpConsumptionRate));
    const currentATP = Math.max(5, Math.min(initialValues.atp, lastATP + deltaATP));
    
    // Calculate ADP
    const currentADP = calculateADP(currentATP, initialValues.pi, Math.pow(10, -currentPH), currentPCr);
    
    // Calculate pyruvate deficit
    const pyruvateDeficit = Math.max(0, atpConsumptionRate - oxidativePhosphorylationRate);
    
    // Store results
    simulationResults.time.push(simulationState.currentTime);
    simulationResults.power.push(currentPower);
    simulationResults.vo2.push(currentVO2);
    simulationResults.vlamax.push(currentVLAmax);
    simulationResults.vlaox.push(lactateOxidationRate);
    simulationResults.pyruvateDeficit.push(pyruvateDeficit);
    simulationResults.muscleLactate.push(currentMuscleLactate);
    simulationResults.bloodLactate.push(currentBloodLactate);
    simulationResults.pH.push(currentPH);
    simulationResults.pcr.push(currentPCr);
    simulationResults.atp.push(currentATP);
    simulationResults.adp.push(currentADP);
}

// Update UI with current values
function updateUI() {
    const lastIndex = simulationResults.time.length - 1;
    
    currentTimeDisplay.textContent = `${simulationResults.time[lastIndex]} s`;
    currentPowerDisplay.textContent = `${simulationResults.power[lastIndex]} W`;
    currentVO2Display.textContent = `${Math.round(simulationResults.vo2[lastIndex])} ml/min`;
    currentVLAoxDisplay.textContent = `${simulationResults.vlaox[lastIndex].toFixed(3)} mmol/s/kg`;
    currentPyruvateDeficitDisplay.textContent = `${simulationResults.pyruvateDeficit[lastIndex].toFixed(3)}`;
    currentMuscleLactateDisplay.textContent = `${simulationResults.muscleLactate[lastIndex].toFixed(2)} mmol/l`;
    currentBloodLactateDisplay.textContent = `${simulationResults.bloodLactate[lastIndex].toFixed(2)} mmol/l`;
    currentPHDisplay.textContent = `${simulationResults.pH[lastIndex].toFixed(2)}`;
    currentPCrDisplay.textContent = `${simulationResults.pcr[lastIndex].toFixed(2)} mmol/kg`;
    currentATPDisplay.textContent = `${simulationResults.atp[lastIndex].toFixed(2)} mmol/kg`;
}

// Update charts with current data
function updateCharts() {
    // Update power chart
    powerChart.data.labels = simulationResults.time;
    powerChart.data.datasets[0].data = simulationResults.power;
    powerChart.update();
    
    // Update metabolic chart
    metabolicChart.data.labels = simulationResults.time;
    metabolicChart.data.datasets[0].data = simulationResults.vo2;
    metabolicChart.data.datasets[1].data = simulationResults.muscleLactate;
    metabolicChart.data.datasets[2].data = simulationResults.bloodLactate;
    metabolicChart.data.datasets[3].data = simulationResults.pH;
    metabolicChart.data.datasets[4].data = simulationResults.pcr;
    metabolicChart.data.datasets[5].data = simulationResults.atp;
    
    // Update y-axis scale for VO2
    metabolicChart.options.scales['y-vo2'].max = modelParameters.vo2max * 1.1;
    
    metabolicChart.update();
}
