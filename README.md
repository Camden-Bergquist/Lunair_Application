# Lunair IPG Programmer Companion: Data Tool

## Introduction:

Data cleaning and manipulation companion tool for Lunair Medical's IPG programmer written in RShiny.

The purpose of this application is twofold: to read in and clean the programmer's marker log csv file, and to visualize the marker data present in the log. The visualization allows us to get a better look at the marker data over time, as well as ensure that the marker output is the same as what's being shown on the programmer. The cleaning serves as a way to export the data in a format which is much more easy-to-manipulate and read, cleaning it for future use.

On startup, the user will be asked to upload a csv file. This file must be in the exact format exported by the programmer: a semicolon-delimited csv file with column headers `Channel`, `StartTime (MM/dd/yyyy HH:mm:ss.fff)`, `EndTime (MM/dd/yyyy HH:mm:ss.fff)`, and `Value`. Currently, the application is built to handle marker log files from versions 1.0.5 and 1.0.6 of the programmer. Support for version 1.0.7 logs to come.


## Data Cleaning:

### Overview:

The data cleaning tab allows the user to manipulate and output the marker data in a simple-to-read format. It also allows the user to slightly alter the data before export. In order to achieve this, the data gets pivoted so that each channel has a column of its own. The parameters that the user can manipulate before export are as follows:

- A slider input allows the user to select the range of elapsed time (in milliseconds) to include in the output. The slider has a minimum value of zero, and a maximum value based on the final timestep included in the marker log.
- A series of checkboxes allow the user to selectively include or exclude columns in the final output. `Time_Elapsed_MS` cannot be excluded. Attempts to include columns with no values (entirely NA/missing values) will result in an error which can be resolved by simply deselecting the column in question.
- The 'Upload File' button, located in the bottom-left section of the tab, can be used to upload a different file than the one the user is prompted for upon opening the application.

In addition, a dynamic preview of the output is displayed in an interactive data table, which the user can utilize to verify that the exported data will resemble their desired format.

### Output:

Below is a preview of the columns or 'variables' able to be included in the exported file:

| Variable                  | Type      | Description                                                                                                                |
|---------------------------|-----------|----------------------------------------------------------------------------------------------------------------------------|
| `Time_Elapsed_MS`         | Numeric   | Time elapsed (in milliseconds) since the beginning of the therapy.                                                         |
| `IMUAccelerometerX`       | Numeric   | Accelerometer X-axis value.                                                                                                |
| `IMUAccelerometerY`       | Numeric   | Accelerometer Y-axis value.                                                                                                |
| `IMUAccelerometerZ`       | Numeric   | Accelerometer Z-axis value.                                                                                                |
| `IMUGyroscopeX`           | Numeric   | Gyroscope X-axis value.                                                                                                    |
| `IMUGyroscopeY`           | Numeric   | Gyroscope Y-axis value.                                                                                                    |
| `IMUGyroscopeZ`           | Numeric   | Gyroscope Z-axis value.                                                                                                    |
| `TransthoracicImpedance`  | Numeric   | Transthoracic impedance value (must be mathematically converted into ohms).                                                |
| `StimulationWaveform`     | Numeric   | Stimulation waveform value as specified in the programmer.                                                                 |
| `Posture`                 | Character | Dynamically generated column based on accelerometer values. Column added by application; not extant in original dataset.   |

A timestep (observation at a given `Time_Elapsed_MS` value) will only be included in the final output if at least value exists at that timestep for any given included column/variable . In other words, if a row is filled entirely with NAs/missing values, it will *not* be included in the final output.

The manipulated data can be exported by clicking the 'Save File' button, positioned right underneath the 'Upload File' button. The user will be prompted for a file name and file path to save the output to. The file is exported in the format of a comma-delimited csv file.

### Miscellaneous Variable Information:

All numeric values except for `Time_Elapsed_MS` are represented digitally. This means that they must be mathematically transformed before reflecting real world values. For example:

- `TransthoracicImpedance` is, for the purposes of therapy, measured in ohms (Ω). In order to convert the `TransthoracicImpedance` values into ohms, the approximate formula `Ω = 0.04176689(TransthoracicImpedance) - 8.5538812` or the simplified `Ω = 0.42(TransthoracicImpedance) - 8.55` must be applied.
- `IMUAccelerometerX`, `IMUAccelerometerY`, and `IMUAccelerometerZ` are measured in gs, but represented digitally in a range from -16,383 to 16,383 (-(2<sup>14</sup> - 1) to 2<sup>14</sup> - 1), for 1g acting negatively against an axis to 1g acting positively against an axis, respectively. The maximum range of values currently supported by the accelerometer is -32,767 to 32,767 (-(2<sup>15</sup> - 1) to 2<sup>15</sup> - 1), for a maximum reading of positive or negative 2gs.

The `Posture` column is dynamically generated based on `IMUAccelerometerX` and `IMUAccelerometerZ` values. First, an estimation of the patient's incline, from 0&deg; to 180&deg; (where 0&deg; is supine, 90&deg; is upright, and 180&deg; is prone), is generated, and then subdivided into five positions: `Supine`, `Shallow_Incline`, `Steep_Incline`, `Upright`, and `Prone`. Two more positions, `Left_Lateral` and `Right_Lateral`, are then subsequently assigned. Finally, in all cases where one of the aforementioned positions is not assigned, the positions are designated `OOB` for 'out-of-bounds'. Ultimately, the calculations only account for a relatively small range of motion: a patient's incline from 0&deg; to 90&deg;, as well as `Prone`, `Left_Lateral`, and `Right_Lateral`, all three of which are only assigned when a patient is deemed at an incline level below `Shallow_Incline`. 

In practice, this means that a majority of the possible positions the patient could be in are labeled `OOB`. Fast or erratic movement will also cause an `OOB` to register, as the accelerometer values corresponding to the movement will fall outside of their expected range. Furthermore, these calculations assume a specific orientation from the breadboard/IPG, and does not yet support any kind of calibration. All-in-all, these values were created for a test environment, and their limitations reflect that purpose. As such, interpreting patient position for tests which don't take into account a given, standardized position, is infeasable and should not be attempted.

## Data Visualization:

### Overview:

At present, the data visualization tab of the application renders four interactive graphs, which can be selected from a dropdown menu at the top of the tab. The four currently available visualization options are:

- `Accelerometer`
- `Gyroscope`
- `Stimulus`
- `Posture`

Interactive graph functions:

- In cases where there are multiple traces (which is what we call lines) on a single graph, the user can double-click on one of the traces in the graph's legend to isolate it. Alternatively, single-clicking on one of the traces disables that specific trace. 
- Sections of the graph can be zoomed-in to by dragging horizontally, vertically, or diagonally with the mouse. Zoom options can also be found in the toolbar on the top-right of the graph.
- Double clicking on any point on the graph will zoom-out and reset it to its default render. This option is also present under 'reset axes' on the toolbar.
- The user can pan the graph by selecting the 'pan' tool in the toolbar before dragging the graph in any direction.
- The 'camera' icon, located furthest left on the toolbar, allows the user to download a png of the current render of the graph (zoom/pan included).
- Hovering the mouse over a line or data point will display exact X- and Y-values in the form of a tooltip. A toggle option, located furthest right on the toolbar, adjusts the hover-over to instead compare each trace's Y-value at a specific X-value, rather than displaying just one.

### Accelerometer/Gyroscope:

The accelerometer and gyroscope graphs are very similar, with three traces each. The accelerometer graph visualizes `IMUAccelerometerX`, `IMUAccelerometerY`, and `IMUAccelerometerZ` values against `Time_Elapsed_MS`. The gyroscope graph visualizes `IMUGyroscopeX`, `IMUGyroscopeY`, and `IMUGyroscopeZ` values against `Time_Elapsed_MS`. For both graphs, there is only one Y-axis, as all three of their respective traces are measured in the same (albeit digitized) units.


### Stimulus:

The stimulus graph visualizes `StimulationWaveform` and `TransthoracicImpedance` against `Time_Elapsed_MS`. As `StimulationWaveform` and `TransthoracicImpedance` are measured in different units, two Y-axis scales are present. The axis on the left-hand side represents `StimulationWaveform`, while the axis on the right-hand side represents `TransthoracicImpedance`. For the purposes of this graph, `TransthoracicImpedance` values are converted into ohms, using the approximate formula `Ω = 0.04176689(TransthoracicImpedance) - 8.5538812`.

In the case that a marker log dataset does not include both `StimulationWaveform` and `TransthoracicImpedance`, the stimulus graph will account for this and render a graph with just the single trace present in the data.

### Posture:

The posture graph visualizes the categorical variable `Posture` against `Time_Elapsed_MS`. There are eight possible Y-axis values. They are, in order from top-to-bottom: `OOB`, `Upright`, `Steep_Incline`, `Shallow_Incline`, `Supine`, `Right_Lateral`, `Left_Lateral`, and `Prone`. Even though there is a degree of continuity between possible postures (such as from `Supine` to `Upright`), postures should not be treated as continuous, which is to say that it is possible and expected for the graph to jump from any one posture to any other posture, rather than moving incrementally from neighboring posture to neighboring posture.

<br>

# To-Do:

- Add support for logs sourced from programmer version 1.0.7.
  - In particular, look into how to handle the new channels. Two are just numbers --- see if Jim got any information out of Integer from them --- while the other two are posture indicators. The logic should probably be written to ignore everything for now(?), since the posture indicators don't really work in the first place, and the other two channels are unknown agents.
- *Maybe* consider adding logic for some sort of calibration related to posture calculation? Lots of work for little value there.