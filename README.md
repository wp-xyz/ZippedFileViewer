# ZippedFileViewer
A viewer of the raw files contained in a zip file without having to unzip the archive.

![grafik](https://user-images.githubusercontent.com/30792460/216105585-5e32a4fe-396d-4c12-8520-5c4439e958b0.png)

This little application was written as a tool to understand Office file formats (xlsx, docx, odf, etc.) with the intention to extend Lazarus libraries such as fpspreadsheet or fpvectorial. These formats simply are normal zip files with changed extension. The data files of some other applications are stored in the same way, and thus can be analyzed accordingly.

A test file can be selected by means of the '...' button, or by selecting a previously loaded file from the dropdown combobox. The file directory of the archive is displayed in the table at the left. After clicking an item in this list, the associated file is unzipped internally and displayed at the right, depending on the file type either in text view (with syntax highlighting) or as an image. Optionally the views can be switched to hexadecimal display.

The application was developed and tested on Windows 11, but it should work on other platforms, too. It can be compiled with Lazarus v2.0.10 or newer (with Free Pascal v3.2.0 or newer). Installation of additional packages is not required.

__Acknowledgements__
* Lazarus IDE: https://www.lazarus-ide.org/
* Free Pascal compiler: https://www.freepascal.org/
* MPHexEditor: https://github.com/michalgw/mphexeditor
* Additional SynEdit highlighters: https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/extrasyn/
* The icons used are provided by Roland Hahn: https://www.rhsoft.de/, see also https://gitlab.com/freepascal.org/lazarus/lazarus/-/tree/main/images/general_purpose.
