# Boids Flocking Simulation in Pascal

_Version 1.3.0+20220406  ([Version Release Notes](#ReleaseNotes))_ 

**Boids Flocking Simulation** is an open source flock of birds simulation.

## About the Software

The software is a self-contained executable program, written in **[Free Pascal](https://www.freepascal.org/)**, that runs on Microsoft Windows or Ubuntu Linux (and presumably other Linux distributions).
(No separate run-time environment is required to run the program.)
The **[Lazarus Integrated Development Environment](https://www.lazarus-ide.org/)** was used to develop the program.
(Both Free Pascal and the Lazarus IDE are free open-source software products.) 

## Downloading and Running the Program

### Microsoft Windows

You can run the Boids Flocking Simulation program on Microsoft Windows as follows:

- Download the **PascalBoids.exe** binary executable file from the **bin** sub-folder from this GitHub.com page.

- To uninstall the program, simply delete the **PascalBoids.exe** file.

### Ubuntu Linux

You can run the Astronomical Simulation program on Ubuntu Linux (and presumably other Linux distributions) as follows:

- Download the **PascalBoids** binary executable file (with no file extension) from the **bin** sub-folder from this GitHub.com page.

- Ensure the **PascalBoids** file has the executable permission.  From a Files window, right-click the file, select Properties, and use the Permissions tab to enable the Execute permission.  To do this in a Terminal window, use the following command:
  
    chmod +x AstroSim

- To uninstall the program, simply delete the **PascalBoids** binary executable file.

### Running the Program

Double-click the downloaded copy of **PascalBoids.exe** (on Windows) or **PascalBoids** (on Linux) to start the simulation.

When the program starts it displays the **Boids Flocking Simulation** Main Form.

Here is an image of the Main Form paused during a running simulation.

![PascalBoids Form](img/AstroSim.png?raw=true "PascalBoids Form")

The Main Form contains these elements:

### Resizing the Main Form

The initial size of the Main Form is designed to fit within an 800 by 600 monitor window.

To enlarge the form, drag its boundary or simply click the _maximize_ icon (small square in the upper right of the title bar).

Then click the **Randomize** button to enlarge the graphic simulation area to match the enlarged form.

## Source code compilation notes

Download the **Lazarus IDE**, including **Free Pascal**, from  here:

- **<https://www.lazarus-ide.org/index.php?page=downloads>**

After installing the **Lazarus IDE**, clone this GitHub repository to your local disk.
Then double-click on the **src\PascalBoids.lpr** project file to open it in **Lazarus**. 

_**Note:**_ Using the debugger in the **Lazarus IDE** on Windows 10 _**might**_ require the following configuration adjustment:

- **[Lazarus - Windows - Debugger crashing on OpenDialog](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/lazarus-windows-debugger-crashing-on-opendialog/)**

When **Lazarus** includes debugging information the executable file is relatively large.
When ready to create a release executable, the file size can be significantly reduced by selecting the menu item **Project | Project Options ...** and navigating to the **Compile Options | Debugging** tab in the resulting dialog window.
Clear the check-mark from the **Generate info for the debugger** option and then click the **OK** button.
Then rebuild the executable using the **Run | Build** menu item (or using the shortcut key-stroke _**Shift-F9**_).

<a name="ReleaseNotes"></a>

## Release Notes

### Version 1.0.0

This is the initial version of the software.
