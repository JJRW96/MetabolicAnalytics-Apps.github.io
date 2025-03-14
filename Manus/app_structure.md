# HTML App Structure for Mader Model Step Test Simulation

## Overview
This document outlines the structure of the HTML application for simulating a step test using the Mader model. The app will visualize metabolic parameters during a step test with 30-second steps, starting at 60 watts and increasing by 20 watts up to 500 watts.

## Application Components

### 1. HTML Structure
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Mader Model Step Test Simulation</title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <header>
        <h1>Mader Model Step Test Simulation</h1>
    </header>
    
    <main>
        <section id="controls">
            <!-- Parameter sliders and test controls -->
        </section>
        
        <section id="simulation">
            <!-- Main visualization area -->
        </section>
        
        <section id="parameters">
            <!-- Current parameter values display -->
        </section>
    </main>
    
    <footer>
        <p>Based on the Mader model of muscle metabolism</p>
    </footer>
    
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chartjs-plugin-annotation"></script>
    <script src="simulation.js"></script>
</body>
</html>
```

### 2. CSS Structure (styles.css)
```css
/* Main layout and styling */
body {
    font-family: 'Arial', sans-serif;
    line-height: 1.6;
    color: #333;
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
    background-color: #f5f5f5;
}

header {
    text-align: center;
    margin-bottom: 20px;
}

main {
    display: grid;
    grid-template-columns: 1fr 3fr;
    grid-template-rows: auto 1fr;
    grid-template-areas:
        "controls simulation"
        "parameters simulation";
    gap: 20px;
}

#controls {
    grid-area: controls;
    background-color: #fff;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

#simulation {
    grid-area: simulation;
    background-color: #fff;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    height: 600px;
}

#parameters {
    grid-area: parameters;
    background-color: #fff;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

footer {
    text-align: center;
    margin-top: 20px;
    font-size: 0.8em;
    color: #666;
}

/* Slider styling */
.slider-container {
    margin-bottom: 15px;
}

.slider-container label {
    display: block;
    margin-bottom: 5px;
    font-weight: bold;
}

.slider-container input[type="range"] {
    width: 100%;
}

.slider-value {
    display: inline-block;
    width: 50px;
    text-align: right;
}

/* Button styling */
button {
    background-color: #4CAF50;
    color: white;
    border: none;
    padding: 10px 15px;
    text-align: center;
    text-decoration: none;
    display: inline-block;
    font-size: 16px;
    margin: 4px 2px;
    cursor: pointer;
    border-radius: 4px;
    transition: background-color 0.3s;
}

button:hover {
    background-color: #45a049;
}

button:disabled {
    background-color: #cccccc;
    cursor: not-allowed;
}

/* Parameter display styling */
.parameter-group {
    margin-bottom: 15px;
}

.parameter-group h3 {
    margin-bottom: 5px;
    font-size: 1em;
}

.parameter {
    display: flex;
    justify-content: space-between;
    margin-bottom: 5px;
}

.parameter-name {
    font-weight: bold;
}

.parameter-value {
    font-family: monospace;
}
```

### 3. JavaScript Structure (simulation.js)

The JavaScript file will be organized into several modules:

#### a. Constants and Configuration
```javascript
// Constants for the step test protocol
const STEP_DURATION_SECONDS = 30;
const STARTING_POWER_WATTS = 60;
const POWER_INCREMENT_WATTS = 20;
const MAX_POWER_WATTS = 500;

// Default parameter values
const DEFAULT_VO2MAX_ML_MIN = 6000;
const DEFAULT_VLAMAX = 0.70;

// Conversion factors and other constants from the Mader model
// (These will be extracted from the research papers)
```

#### b. Model Parameters and State
```javascript
// Current simulation state
let simulationState = {
    running: false,
    currentTime: 0,
    currentPower: 0,
    currentStep: 0,
    // ... other state variables
};

// Model parameters (can be adjusted by sliders)
let modelParameters = {
    vo2max: DEFAULT_VO2MAX_ML_MIN,
    vlamax: DEFAULT_VLAMAX,
    // ... other adjustable parameters
};

// Simulation results storage
let simulationResults = {
    time: [],
    power: [],
    vo2: [],
    vlaox: [],
    pyruvateDeficit: [],
    muscleLactate: [],
    bloodLactate: [],
    pH: [],
    pcr: [],
    atp: [],
    // ... other result arrays
};
```

#### c. Mader Model Implementation
```javascript
// Implementation of the Mader model equations

// ATP-PCr Equilibrium calculations
function calculateADP(atp, pi, hPlus, pcr) {
    // Implementation of equation: [ADP] = [ATP] * [Pi] / (M2 * [H+] * [PCr])
    // ...
}

// Oxidative Phosphorylation calculations
function calculateOxidativePhosphorylation(adp, deltaG) {
    // Implementation based on Mader model
    // ...
}

// Glycolysis calculations
function calculateGlycolysis(adp, amp, pH) {
    // Implementation based on Mader model
    // ...
}

// Lactate dynamics calculations
function calculateLactateDistribution(muscleLactate, bloodLactate) {
    // Implementation of two-compartment model
    // ...
}

// pH calculations
function calculatePH(lactate, bufferingCapacity) {
    // Implementation based on Mader model
    // ...
}

// Main simulation step function
function simulationStep(deltaTime) {
    // Calculate all metabolic parameters for the current time step
    // ...
}
```

#### d. Step Test Simulation Logic
```javascript
// Step test protocol implementation
function startStepTest() {
    // Initialize simulation
    resetSimulation();
    simulationState.running = true;
    simulationState.currentPower = STARTING_POWER_WATTS;
    
    // Start simulation loop
    simulationLoop();
}

function simulationLoop() {
    // Main simulation loop
    if (!simulationState.running) return;
    
    // Update time
    simulationState.currentTime += 1; // 1 second increment
    
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
    
    // Calculate metabolic parameters for this time point
    simulationStep(1); // 1 second time step
    
    // Store results
    storeResults();
    
    // Update visualization
    updateCharts();
    
    // Continue simulation loop
    requestAnimationFrame(simulationLoop);
}

function stopSimulation() {
    simulationState.running = false;
    // Additional cleanup if needed
}

function resetSimulation() {
    // Reset all simulation state and results
    // ...
}
```

#### e. Visualization and UI
```javascript
// Chart initialization
let powerChart;
let metabolicChart;

function initializeCharts() {
    // Create Chart.js instances for visualization
    // ...
}

function updateCharts() {
    // Update charts with latest simulation data
    // ...
}

// UI event handlers
function initializeUI() {
    // Set up sliders, buttons, and other UI elements
    // ...
}

function updateParameterDisplay() {
    // Update the display of current parameter values
    // ...
}

// Initialize everything when the page loads
window.addEventListener('DOMContentLoaded', () => {
    initializeUI();
    initializeCharts();
    updateParameterDisplay();
});
```

## Data Flow

1. User adjusts parameters using sliders (VO2max, VLamax)
2. User starts the simulation
3. Step test protocol runs, increasing power every 30 seconds
4. For each time step:
   - Current power is determined based on the step test protocol
   - Metabolic parameters are calculated using the Mader model equations
   - Results are stored in arrays
   - Visualization is updated
5. Simulation continues until max power is reached or user stops it
6. Results are displayed in charts with multiple y-axes

## Visualization Design

The main visualization will consist of two synchronized charts:

1. **Power Chart**: Shows power output over time
   - X-axis: Time (seconds)
   - Y-axis: Power (watts)

2. **Metabolic Parameters Chart**: Shows all metabolic parameters over time
   - X-axis: Time (seconds) (synchronized with Power Chart)
   - Multiple Y-axes:
     - VO2ss (ml/min)
     - VLamax and VLaox (mmol/s/kg)
     - Pyruvate deficit
     - Muscle lactate (mmol/l)
     - Blood lactate (mmol/l)
     - pH
     - PCr (mmol/kg)
     - ATP (mmol/kg)

Each parameter will have a distinct color and line style for clear differentiation.

## User Controls

1. **Parameter Sliders**:
   - VO2max slider (range: 2000-8000 ml/min, default: 6000 ml/min)
   - VLamax slider (range: 0.2-1.5, default: 0.70)
   - Additional parameter sliders as needed

2. **Simulation Controls**:
   - Start button
   - Stop button
   - Reset button

3. **Parameter Display**:
   - Current values of all metabolic parameters
   - Updated in real-time during simulation

## Next Steps

1. Implement the HTML structure
2. Create the CSS styling
3. Implement the JavaScript simulation logic
4. Test the application with different parameter values
5. Optimize performance and user experience
