@device(postscript)
@Make(manual)
@disable(figurecontents)
@LibraryFile(Garnet)
@String(TitleString = "Demonstration Programs")
@Use(Bibliography = "garnet.bib")
@begin(TitlePage)

@comment(This is added because there are no chapters in this doc.)
@Modify(Section, TitleForm
{@Begin(HD2)@parm(Numbered) @parm(Title)@End(HD2)
@string(SectionNumber @parm(Referenced))
@string(SectionTitle @parm(Title))
@PageHeading(Even, left "@ux[Page @value(page)@hsp(0.15 in)@Value(TitleString) @>@parm(Title)@hsp(0.25 in)Section @parm(Referenced)]")
@PageHeading(Odd,Immediate,Left "@ux[Section @parm(Referenced)@hsp(0.25 in) @Parm(Title) @> Page @value(page)]")})

@begin(TitleBox)
@blankspace(0.6 inch)
@Bg(Demonstration Programs for Garnet)

@b(Brad A. Myers
Andrew Mickish)
@BlankSpace(0.3 line)
@value(date)
@end(TitleBox)
@BlankSpace(0.5 inch)
@center[@b(Abstract)]
@begin(Text, spacing=1.1)
This file contains an overview of the demonstration programs distributed
with the Garnet toolkit.  These programs serve as examples of what Garnet
can do, and also of how to write Garnet programs.

@blankspace(0.5 inch)
@include(creditetc.mss)
@End(Text)
@end(TitlePage)


@include(pagenumbers.mss)
@set(page = demos-first-page)

@Section(Introduction)
Probably the best way to learn about how to code using the Garnet
Toolkit is to look at example programs.  Therefore, we have provided a
number of them with the Toolkit release.  In addition, you can load and run
the demos to see what kinds of things Garnet can do. 

The ``best'' example program is @pr(demo-editor), which is included in this
technical report.  The other example programs serve mainly to show how
particular special features of Garnet can be used.

Unfortunately, many of the demonstration programs were implemented before
important parts of the Garnet Toolkit were implemented.  For example, many
of the demos do not use Aggregadgets and Aggrelists.  These
particular demos are
@i(not) good examples of how we would code today.  Hopefully, we will soon
re-code all of these old demos using the newest features, but for the time
being, you will probably only want to look at the code of the newer demos.

This document provides a guide to the demo programs, what they are supposed
to show, and whether they are written with the latest style or not.


@Section(Loading and Compiling Demos)
@label(loadingandcompilingdemos)

@Index(Compiling demos)
If for some reason the demos were not compiled during the standard installation
procedure discussed in the Overview Manual, you can compile just the demos
by executing (garnet-load "demos-src:demos-compiler").  This will generate
new binaries for the demos, which will need to be copied from the
@pr(src/demos/) directory into your @pr(bin/demos) directory.

Normally, the demonstration programs are @i(not) loaded by the standard
Garnet loader.  The best way to view the demos is to load the
@pr(garnet-loader) as usual and then load the Demos Controller:
@begin(programexample)
(garnet-load "demos:demos-controller")
(demos-controller:do-go)
@end(programexample)

This will load the controller itself, but not any of the demos.  It will
display a window with a set of check buttons in it.  Just click with the mouse
on a button, and the corresponding demo will be loaded and started.
Clicking on the check box again will stop the demo.  Clicking again will
restart it (but not re-load it).  An instruction window will appear at the
bottom of the screen with the instructions for the last demo started.

The @pr(demos-controller) application features the @pr(gg:mouseline) gadget.
When you keep the mouse still over one of the x-buttons for about 2 seconds,
a window will pop up with a short description of the corresponding demo.
For more information about this gadget, see the appropriate section of the
Gadgets Manual.

Using the @pr(demos-controller) causes each demo file to be loaded as it is
needed.  If you wanted to load @u(all) of the demos at once (whether you
eventually planned to use the @pr(demos-controller) or not), you could set
@pr(user::load-demos-p) to be T before loading @pr(garnet-loader), or
execute @pr(load Garnet-@|Demos-@|Loader).

All of the demos described here are in the sub-directory @pr(demos).

@Section(Running Demo Programs)
@index(running demos)
@index(starting demos)
@index(stopping demos)
@index(do-go)
@index(do-stop)

To see a particular demo program, it is not necessary to use the Demos
Controller described in section @ref(loadingandcompilingdemos).
Instead, the file can be loaded and executed by itself.

Almost all of the demonstration programs operate the same way.  Once a file
@pr(demo-)@i(xxx) is loaded, it creates a package called @pr(demo-)@i(xxx).
In this package are two procedures -- @pr(do-go) to start the demo and
@pr(do-stop) to stop it.  Therefore, to begin a demo of @i(xxx), you would
type: @pr[(demo-@i(xxx):do-go)].  The @pr(do-stop) procedure destroys the
window that the demo is running in.  You can load and start as many demos
as you like at the same time.  Each will run in its own separate window.  

The @pr(do-go) procedure will print instructions in the Lisp window about
how to operate the demonstration program.

Demos for the individual gadgets are all in the @pr[garnet-gadgets] package
and have unique names.  Section @ref(gadgetdemos) describes how to see
these demos.


@Section(Double-Buffered Windows)
@index(double-buffered windows)

All the demos can take advantage of the Opal feature for double-buffered
windows.  The @pr(do-go) routine for each demo has an optional
@pr(:double-buffered-p) argument that defaults to NIL.  For instance, to
run @pr(demo-3d) on a double-buffered window, say:
@programexample[(demo-3d:do-go :double-buffered-p T)]
and to run it normally, say:
@programexample[(demo-3d:do-go)]


@Section(Best Examples)

@SubSection(GarnetDraw)
@index(Garnetdraw)
@index(Drawing program)
There a useful utility called @pr(GarnetDraw) which is a
relatively simple drawing program written using Garnet.  Since the file format
for storing the created objects is simply a Lisp file which creates
aggregadgets, you might be able to use GarnetDraw to prototype
application objects (but Lapidary is probably better for this).

GarnetDraw uses many features of Garnet including gridding, PostScript
printing, selection of all objects in a region, moving and growing of multiple
objects, menubars, and the @pr(save-gadget) and @pr(load-gadget) dialog boxes.
The editing functions like Cut, Copy, and Paste are
implemented using the @pr(Standard-Edit) module from @pr(garnet-gadgets),
and objects can be cut and pasted between @pr(GarnetDraw) and @pr(Gilt)
(since they share the same clipboard).  Accelerators are defined for the
menubar commands, like @pr(META-x) for Cut and
@pr(META-v) for Paste.

GarnetDraw works like most Garnet programs: select in the palette
with any button, draw in the main window with the right button, and select
objects with the left button.  Select multiple objects with shift-left or
the middle mouse button.  Change the size of objects by pressing on black
handles and move them by pressing on
white handles.  The line style and color and filling color can be
changed for the selected object and for further drawing by clicking on
the icons at the bottom of the palette.
You can also edit the shape of polylines:
create a polyline, select it, and choose "Reshape" from the "Edit" menu.

@SubSection(Demo-Editor)
@label(demoeditor)
@Index(demo-editor)
Probably the best example program is the sample graphics editor in the
file @pr(demo-editor.lisp).
It demonstrates many of the basic components when building a Garnet
application.  This demo automatically loads and uses the
@pr(text-button-panel), @pr(graphics-selection), and @pr(arrow-line)
gadgets.


@SubSection(Demo-Arith)
@index(demo-arith)
@index(Postscript in demo-arith)
@index(Gestures in demo-arith)

@pr(Demo-arith) is a simple visual programming interface for constructing
arithmetic expressions.  It uses constraints to solve the expressions.
There are buttons for producing PostScript output from the picture.
Also, you can create new objects using gestures by dragging with the
middle mouse button (rather than selecting them from the palette).
The instructions are printed when the program is started.



@SubSection(Demo-Grow)
@label(demogrow)
@Index(demo-grow)
@Index(graphics-selection)
@pr(Demo-grow) shows how to use the @pr(graphics-selection) gadget.
It uses the same techniques as in @pr(demo-editor) (section @ref(demoeditor)).


@SubSection(Multifont and Multi-Line Text Input)
@index(text-interactor)
@index(multi-line text input)
@index(multifont text input)
@index(demo-text)

@pr(Demo-text) shows how multi-line, multi-font text input can be handled.  
It does not use Aggregadgets or any gadgets, but none are necessary.


@SubSection(Demo-Multifont)
@index(demo-multifont)

To see how to effectively use the multifont text object, along with its
interactors, examine the @pr(demo-multifont) demo.  Most of the code
is actually a good demonstration of how to use the @pr[menubar] and
@pr[motif-scrolling-window-with-bars] gadgets, but the
@pr[multifont-text] objects and interactors are in there.  Features
demonstrated include word wrap and how to changing the fonts with the special
multifont accelerators.

The @pr(lisp-mode) feature of @pr(multifont-text) is also shown in this
demo.  Select "Toggle Lisp Mode" from the "Edit" menu, and type in a lisp
expression (like a @pr[defun] definition).  As you hit return, the next line
will be automatically indented according to standard lisp conventions.  Hitting
the tab key will re-indent the current line.


@SubSection(Creating New Objects)
@Index(creating new objects)
@Index(demo-twop)
@Index(two-point-interactor)

@pr(Demo-twop) shows how new lines and new rectangles can be input.  It
uses the same techniques as in @pr(demo-editor) (section @ref(demoeditor)).

@SubSection(Angles)
@index(Angle-Interactor)
@index(demo-angle)
@index(gauge)
There are two programs that demonstrate how to use the angle interactor.
@pr(Demo-angle) contains circular gauges (but see the @pr(gauge)
gadget@dash@;section @ref(gadgetdemos)), as
well as a demonstration of how to use the ``angle-increment''
parameter to the angle @pr(:running-action) procedure.

@index(demo-clock)
@index(clock)
@pr(Demo-clock) shows a clock face with hands that can be rotated with the
mouse.


@SubSection(Aggregraphs)
@index(aggregraphs)
@index(demo-graph)
The @pr(demo-graph) file is an example of many features of Aggregraphs.


@SubSection(Scroll Bars)
@Index(scroll bars)
Although sliders and scroll bars are provided in the Garnet Gadget set
(the @pr(gadgets) subdirectory), the file @pr(demo-scrollbar) contains
some alternative scroll bar objects.  The Macintosh scroll bar in this demo
was written in the old Garnet style, but there are new versions of scroll bars
in the OpenLook, Next, and Motif style.

To see the demo of all four scroll bars, use the functions
@pr(demo-scrollbar:do-go) and @pr(demo-scrollbar:do-stop) as usual.  There
are also functions that display the scroll bars individually called
@pr(mac-go), @pr(open-go), @pr(next-go), and @pr(motif-go).


@SubSection(Menus)
@index(demo-menu)
@pr(Demo-menu) shows a number of different kinds of menus that can be
created using Garnet.  All of them were implemented using Aggregadgets and
Aggrelists.


@SubSection(Animation)
@index(animation)

@index(demo-animator)
@pr(Demo-animator) uses background animation processes to move several objects
in a window.  One of the objects is a walking figure which moves across the 
screen by rapidly redrawing a pixmap.

@index(demo-fade)
@pr(Demo-fade) shows a simple animation for the Garnet acronym.

@pr(Demo-logo) performs the same animation as @pr(demo-fade), but it
also includes the Garnet logo.


@begin(group)
@SubSection(Garnet-Calculator)

@index(garnet-calculator) @index(demo-calculator) @index(calculator)
The @pr(garnet-calculator) has the look and feel of @pr(xcalc), the calculator
supplied by X windows, but it is more robust.  The calculator is a
self-contained tool, and can be integrated inside a larger Garnet application.

You can load the demo with @pr[(garnet-load "demos:garnet-calculator")].
To run it, execute @pr[(garnet-calculator:do-go)].
@end(group)
@blankspace(1 line)

@index(start-calc) @index(stop-calc)
@begin(programexample)
garnet-calculator:Start-Calc &key @i{double-buffered-p} @value(function)

garnet-calculator:Stop-Calc @i{app-object} &optional (@i{destroy-app-object?} T) @value(function)
@end(programexample)

The function @pr(start-calc) creates and returns a calculator
"application object" that can be used by a larger interface, and this object
should be passed as the @i(app-object) parameter to @pr(stop-calc).




@SubSection(Browsers)
@index(browser-gadget)
@index(demo-schema-browser)
@index(demo-file-browser)
The files @pr(demo-schema-browser) and @pr(demo-file-browser) show two
uses of the @pr(browser-gadget).


@SubSection(Demo-Virtual-Agg)
@index(Demo-Virtual-Agg)

To show off an example of virtual-aggregates, load Demo-Virtual-Agg and say:

@begin(programexample)
(demo-virtual-agg:do-go :num-dots 1000)
@end(programexample)

@pr(Demo-virtual-agg:do-go) takes a single optional keyed parameter
@pr[:num-dots] which tells how many circles should appear in a window.
The default is 1000.

The first 1000 circles are read in from circles.data in the
@pr[user::Garnet-DataFile-PathName] directory (because that's
faster) and the rest are chosen randomly.  A '.' is printed out for
every ten circles.

You will also see a little star in the upper left on the screen, in front
of the @pr[virtual-aggregate], and a big gray rectangle underneath the
@pr[virtual-aggregate].  These are just to show that the update
algorithm is working reasonably well.

@begin(description, leftmargin=10, indent=-6)
Clicking with the left button creates a new circle (of random radius and
    color) where you clicked.

Clicking with the right button "destroys" the top-most circle underneath
    where you clicked, or beeps if there was nothing under there.

Clicking on the little star and dragging moves the little star.

Clicking shift-middle causes the circle underneath the cursor to change
to a different random color. (This shows off @pr[change-item].)

Clicking shift-right causes the entire @pr[virtual-aggregate]
to disappear or reappear.
@end(description)


@SubSection(Demo-Pixmap)
@index(demo-pixmap)

This new demo shows a two-dimensional @pr[virtual-aggregate] in action.
Here, the @pr[virtual-aggregate] is a 50 X 50 array of 5 X 5 rectangles.
Each rectangle can be colored from the color palette, and the pattern of
colored rectangles is reflected in a pixmap.

You can load a pixmap into the demo (e.g., from the directory
@pr(Garnet-Pixmap-PathName)), edit the pixmap with the color palette and
virtual-aggregate, and then save the pixmap to a new file.  You can also
generate PostScript files from this demo, though you have to have a Level 2
printer (that defines the PostScript function @pr(colorimage)) to print
a color pixmap image.



@SubSection(Demo-Gesture)
@index(demo-gesture)
@pr(Demo-gesture) is an example of how the new gesture-interactor can be used
in an interface.  In this demo, you can create perfect circles and
rectangles by drawing rough approximations with the mouse, which are
interpreted by the gesture recognizer.  Gestures may also be used to copy
and delete the shapes you have created.

@SubSection(Demo-Unidraw)
@index(demo-unidraw)
@pr(Demo-Unidraw) is a gesture-based text editor, which allows you to enter
characters with freehand drawing using the mouse.  The gestures that
this demo understands are comprised of a shorthand alphabet devised by
David Goldberg at Xerox Parc.  The gesture patterns are shown in the middle
of the demo window, and the canvas for drawing gestures is at the bottom.
As the demo recognizes the gestures you draw, it selects the corresponding
gesture and puts the new character in the text window.


@SubSection(Gadget Demos)
@Index(gadgets)
@label(gadgetdemos)

@Index(demo-gadgets)
@Index(demo-motif)
There are separate demo programs of some of the gadgets in the files
@pr(demo-gadgets) and @pr(demo-motif).  Each of these packages export the
usual @pr(do-go) and @pr(do-stop) procedures, and can be found in the
@pr(demos) directory.

Other good examples are the Garnet Gadgets, stored in the @pr(gadgets)
sub-directory.  These were @i(all) written using the latest
Garnet features.  At the end of almost all gadget files is a small demo
program showing how to use that gadget.  Since all the gadgets are
in the same package (@pr[garnet-gadgets]), the gadget demos all have
different names.  They are:
@Begin(itemize, spread 0)
@Pr(Arrow-line-go, Arrow-line-stop) @index(arrow-line-go)
- to demonstrate arrow-lines

@pr(Error-gadget-go, Error-gadget-stop) @index(error-gadget)
@index(query-gadget) @index(error-gadget-go)
- to demonstrate both the error gadget and the query gadget

@pr(Gauge-go, Gauge-stop) @index(gauge) @index(gauge-go)
- to demonstrate circular gauges

@pr(H-scroll-go, H-scroll-stop) @index(H-scroll-go)
- to demonstrate standard horizontal scroll bars

@pr(H-slider-go, H-slider-stop) @index(H-slider-go)
- to demonstrate standard horizontal sliders

@pr(Labeled-box-go, Labeled-box-stop) @index(Labeled-Box-go)
- to demonstrate labeled text-type-in objects

@pr(Menu-go, Menu-stop) @index(Menu-go)
- to demonstrate a standard menu

@pr(Menubar-go, Menubar-stop) @index(Menubar-go)
- to demonstrate pull-down menus

@pr(Motif-Check-Buttons-go, Motif-Check-Buttons-stop)
@index(Motif-Check-Buttons-go)
- to demonstrate Motif style check buttons

@pr(Motif-Error-Gadget-go, Motif-Error-Gadget-stop) @index(motif-error-gadget)
@index(motif-query-gadget) @index(motif-error-gadget-go)
- to demonstrate both the motif error gadget and the motif query gadget

@pr(Motif-Gauge-go, Motif-Gauge-stop)
@index(Motif-Gauge-go)
- to demonstrate the Motif style gauge

@pr(Motif-H-Scroll-go, Motif-H-Scroll-stop)
@index(Motif-H-Scroll-go)
- to demonstrate Motif style horizontal scroll bars

@pr(Motif-Menu-go, Motif-Menu-stop)
@index(Motif-Menu-go)
- to demonstrate the Motif style menus

@pr(Motif-Menubar-go, Motif-Menubar-stop) @index(Motif-Menubar-go)
- to demonstrate the Motif style menubar, with accelerators

@pr(Motif-Option-Button-go, Motif-Option-Button-stop)
@index(motif-option-button-go)
- to demonstrate the Motif style version of this popup menu gadget, whose
button changes labels according to the menu selection

@pr(Motif-Radio-Buttons-go, Motif-Radio-Buttons-stop)
@index(Motif-Radio-Buttons-go)
- to demonstrate Motif style radio buttons

@pr(Motif-Scrolling-Labeled-Box-go, Motif-Scrolling-Labeled-Box-stop)
@index(Motif-Scrolling-Labeled-Box-go)
- to demonstrate the Motif style text-type-in field

@pr(Motif-Scrolling-Window-With-Bars-go, Motif-Scrolling-Window-With-Bars-stop)
@index(Motif-Scrolling-Window-go)
- to demonstrate the Motif style scrolling window gadget

@pr(Motif-Slider-go, Motif-Slider-stop)
@index(Motif-Slider-go)
- to demonstrate the vertical Motif slider

@pr(Motif-Text-Buttons-go, Motif-Text-Buttons-stop)
@index(Motif-Text-Buttons-go)
- to demonstrate Motif style text buttons

@pr(Motif-Trill-go, Motif-Trill-stop) @index(motif-trill-go)
- to demonstrate the Motif style trill device

@pr(Motif-V-Scroll-go, Motif-V-Scroll-stop)
@index(Motif-V-Scroll-go)
- to demonstrate the Motif vertical scroll bar

@pr(Mouseline-go, Mouseline-stop) @index(mouseline-go)
- to demonstrate the mouseline and "balloon help" string

@pr(Multifont-Gadget-go, Multifont-Gadget-stop) @index(multifont-gadget-go)
- to demonstrate the gadget which is a conglomeration of a multifont-text,
a focus-multifont-textinter, and a selection-interactor

@pr(Option-Button-go, Option-Button-stop) @index(option-button-go)
- to demonstrate this kind of popup menu gadget, whose button label changes
according to the menu selection

@pr(Popup-Menu-Button-go, Popup-Menu-Button-stop) @index(popup-menu-button-go)
- to demonstrate this kind of popup menu gadget, whose button label is fixed
and may be a bitmap or other object

@pr(Prop-Sheet-For-Obj-go, Prop-Sheet-For-Obj-stop)
@index(prop-sheet-for-obj-go)
- to demonstrate how prop-sheets can be used to change slot values of Garnet
objects

@pr(Radio-Buttons-go, Radio-Buttons-stop) @index(Radio-Buttons-go)
- to demonstrate radio buttons

@pr(Scrolling-Input-String-go, Scrolling-Input-String-stop)
@index(Scrolling-Input-String-go)
- to demonstrate the scrolling input string gadget

@pr(Scrolling-Labeled-Box-go, Scrolling-Labeled-Box-stop)
@index(Scrolling-Labeled-Box-go)
- to demonstrate the standard scrolling labeled box

@pr(Scrolling-Menu-go, Scrolling-Menu-stop)
@index(Scrolling-Menu-go)
- to demonstrate the scrolling menu gadget

@pr(Scrolling-Window-go, Scrolling-Window-stop)
@index(Scrolling-Window-go)
- to demonstrate the standard scrolling window

@pr(Scrolling-Window-With-Bars-go, Scrolling-Window-With-Bars-stop)
@index(Scrolling-Window-go)
- to demonstrate the scrolling window with attached vertical and horizontal
scroll bars

@pr(Text-Buttons-go, Text-Buttons-stop) @index(Text-Buttons-go)
- to demonstrate buttons with labels inside

@pr(Trill-go, Trill-stop) @index(Trill-go)
- to demonstrate the trill-device gadget

@pr(V-scroll-go, V-scroll-stop) @index(V-scroll-go)
- to demonstrate standard vertical scroll bars

@pr(V-slider-go, V-slider-stop) @index(V-slider-go)
- to demonstrate standard vertical sliders

@pr(X-Buttons-go, X-Buttons-stop) @index(X-Buttons-go)
- to demonstrate X buttons


@End(itemize)
Each of these has its own loader file, named something like
@i(xxx)@pr(-loader) for gadget @i(xxx).  See the Gadgets manual for a
table of loader file names.



@SubSection(Real-Time Constraints and Performance)
The program @pr(demo-manyobjs) was written as a test of how fast the
system can evaluate constraints.  The @pr(do-go) procedure takes an
optional parameter of how many boxes to create.  Each box is composed of
four Opal objects.


@Section(Old Demos)

@SubSection(Moving and Growing Objects)

The best example of moving and growing objects is @pr(demo-grow) (section
@ref(demogrow)).

@index(demo-moveline)
@index(move-grow-interactor)
In addition, @pr(demo-moveline) shows how the @pr(move-grow-interactor) can
be used to move either end of a line.

@SubSection(Menus)

@index(demo-3d)
@pr(Demo-3d) shows some menus and buttons where the item
itself moves when the user presses over it, in order to simulate a floating
button.



@Section(Demos of Advanced Features)

@SubSection(Using Multiple Windows)

@index(demo-multiwin)@Index(Multiple Windows)@Index(windows)
@index(demo-multiwin)
@pr(Demo-multiwin) shows how an interactor can be used to move objects
from one window to another.  For more information, see the Interactors manual.

@SubSection(Modes)
@index(Active slot)@Index(demo-mode)@Index(modes)

@pr(Demo-mode) shows how you can use the @pr(:active) slot of an interactor
to implement different modes.  For more information, see the Interactors manual.

@SubSection(Using Start-Interactor)

@index(demo-sequence)
@index(start-interactor)
@pr(Demo-sequence) shows how to use the @pr(inter:start-interactor)
function to have one interactor start another interactor without waiting
for the second one's start event.  Another example of the use of
@pr(inter:start-interactor) is in @pr(demo-editor) (section
@ref(demoeditor)) to start editing the text label after drawing a box.
For more information on @pr(start-interactor), see the Interactors manual.

