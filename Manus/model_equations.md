# Key Equations and Parameters from Mader Model

## Overview
Based on the analysis of the Mader 2003 paper and the Heck 2022 paper, I've identified the key equations and parameters needed to implement the Mader model for simulating metabolic parameters during a step test.

## Metabolic Parameters to Model
1. VO2ss (Steady state oxygen uptake)
2. VLamax (Maximum rate of lactate formation)
3. VLaox (Rate of lactate elimination by oxidation)
4. Pyruvate deficit
5. Muscle lactate accumulation
6. Blood lactate accumulation
7. pH
8. PCr (Phosphocreatine) values
9. ATP values

## Key Equations

### ATP-PCr Equilibrium
The equilibrium of the cytosolic high energy phosphate system is described by:

1. [ATP]/[ADP] = M1 * [PCr]/[Pi]
   - M1 is the apparent equilibrium constant
   - M1 = M2 * [H+] where M2 = 1.66 * 10^9 mol^-1

2. [ADP] = [ATP] * [Pi] / (M2 * [H+] * [PCr])
   - This equation allows calculation of free [ADP]

3. [ATP] * [AMP] = M3 * [ADP]^2
   - M3 is the equilibrium constant (0.85-1.05)
   - [AMP] * [ADP] is approximately equivalent to [ADP]^3

4. ΔGATP,cyt = ΔG0 + R * T * ln([ATP]/([ADP] * [Pi]))
   - ΔGATP,cyt represents the free energy of the CHEP system
   - ΔG0 is the standard free energy of the [ATP]/[PCr] system (-30.6 kJ/mol)

### Oxidative Phosphorylation (OxP)
The rate of oxidative phosphorylation can be calculated as a function of:
1. Free [ADP] as the substrate
2. A second driving force FDG resulting from the difference of free energy ΔGox,ap - ΔGATP,cyt

### Glycolysis
Regulation of glycolysis is calculated as a function of:
1. Free [ADP] and [AMP] at the level of PFK
2. PFK is inhibited by decreasing pH resulting from lactate accumulation

### Lactate Distribution and Elimination
Calculated using a two-compartment model:
1. Active lactate producing space (muscle)
2. Passive space including lactate elimination by combustion

## Important Parameters

### Oxygen Uptake
- VO2: Oxygen uptake (ml/min or ml/min/kg body mass)
- VO2,a: Current VO2 (ml/s/kg muscle)
- VO2,a,rest: Current VO2 at rest (ml/s/kg muscle)
- VO2max: Maximal oxygen uptake (ml/s/kg muscle or body mass)
- VO2,ss: Steady state of VO2 (ml/s/kg muscle mass wet weight)
- tVO2: Time constant (seconds) of the rise of VO2 from the beginning of exercise

### Lactate Parameters
- vla: Rate of lactic acid formation (mmol/s/kg)
- vla,max: Maximal rate of glycolysis expressed as rate of lactic acid formation (mmol/s/kg)
- vla,max,pH: pH Dependent maximal rate of glycolysis (mmol/s/kg)
- vla,ox: Rate of lactate elimination by oxidation (mmol/min/kg)
- vla,ox,b: Rate of lactate elimination by oxidation in blood (mmol/min/kg)
- vla,ox,m: Rate of lactate elimination by oxidation in muscle (mmol/min/kg)
- vla,ss: Steady-state rate of gross lactic acid formation at pH above 7.4 (mmol/s/kg)
- vla,ss,pH: pH dependent steady-state gross rate of formation of lactic acid (mmol/s/kg)
- [la-]: Concentration of lactate (mmol/l)
- [la-]b: Concentration of lactate in blood (mmol/l)
- [la-]m: Concentration of lactate in muscle (mmol/l)
- [la-]ss: Steady state concentration of lactate during equilibrium of lactate production and oxidative elimination

### pH and Related Parameters
- pH: -log10[H+]
- pHm: Intracellular cytosolic pH
- dbuff: Coefficient equal to the mean buffering capacity of skeletal muscle

### PCr and ATP Parameters
- [PCr]: Concentration of phosphocreatine (mmol/kg muscle)
- [ATP]: Concentration of cytosolic adenosine triphosphate (mmol/kg)
- [ADP]: Concentration of cytosolic adenosine diphosphate (mmol/kg)
- [AMP]: Concentration of adenosine monophosphate (mmol/kg)
- [GP]: Sum of concentrations of high energy phosphate compounds ([GP]=[ATP]+[PCr]) (mmol/kg)

### Power Output Parameters
- vGP,E: Power output expressed in terms of rate of GP consumption during contraction (mmol/s/kg)
- vGP,rest: Power output expressed in terms of rate of GP consumption at rest (mmol/s/kg)
- bPow: Rate of GP consumption per watt (mmol/kg/s/watt)

## Extended Mader Model (from Heck 2022)
The extended Mader model includes:
1. Two-compartment system for lactate (muscle and blood)
2. Consideration of hydrogen ion concentration
3. Delayed increase in oxygen supply (O2 deficit)
4. Closed feedback loop of ATP consumption and ATP resynthesis

## Step Test Protocol (User Requirements)
- 30-second steps
- Starting power: 60 watts
- Increment: 20 watts per step
- Maximum power: 500 watts
- Default VO2max: 6000 ml/min
- Default VLamax: 0.70

## Next Steps
1. Implement the mathematical model in JavaScript
2. Create the step test simulation logic
3. Calculate all required metabolic parameters
4. Develop interactive visualization with multiple y-axes
5. Add sliders for VO2max and VLamax adjustment
