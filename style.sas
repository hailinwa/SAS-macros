/* Style definitions
/* by Zhen-Huan Hu
/* Last update: 2019-04-15
/* -------------------------------------- */

ods path work.templat(update) sasuser.templat(read) sashelp.tmplmst(read);

%macro init_style(
  fontsize = 11,
  docfont = Calibri,
  frmfont = Calibri,
  imgfont = Calibri,
  fixfont = Consolas
  );

  %let huge = %eval(&fontsize + 2);
  %let large = %eval(&fontsize + 1);
  %let normalsize = &fontsize;
  %let small = %eval(&fontsize - 2);
  %let tiny = %eval(&fontsize - 4);

  proc template;
    define style styles.kenyrtf;
      parent = styles.rtf;
      class fonts /
      'TitleFont'           = ("&docfont", &normalsize.pt, bold) /* Titles, endnotes */
      'TitleFont2'          = ("&docfont", &normalsize.pt) /* Procedure titles */
      'StrongFont'          = ("&docfont", &normalsize.pt, bold)
      'EmphasisFont'        = ("&docfont", &normalsize.pt, italic)
      'HeadingEmphasisFont' = ("&docfont", &normalsize.pt, bold italic)
      'HeadingFont'         = ("&docfont", &normalsize.pt, bold) /* Table headings */
      'DocFont'             = ("&docfont", &normalsize.pt) /* Table cells */
      'FootFont'            = ("&docfont", &small.pt) /* Footnotes */
      'FixedEmphasisFont'   = ("&fixfont", &small.pt, italic)
      'FixedStrongFont'     = ("&fixfont", &small.pt, bold)
      'FixedHeadingFont'    = ("&fixfont", &small.pt, bold)
      'BatchFixedFont'      = ("&fixfont", &tiny.pt)
      'FixedFont'           = ("&fixfont", &small.pt);

      class graphfonts /
      'GraphTitleFont'      = ("&imgfont", &huge.pt, bold) /* All titles */
      'GraphTitle1Font'     = ("&imgfont", &huge.pt)
      'GraphFootnoteFont'   = ("&imgfont", &large.pt, italic) /* All footnotes */
      'GraphLabelFont'      = ("&imgfont", &large.pt, bold) /* Axis labels, legend titles */
      'GraphLabel2Font'     = ("&imgfont", &large.pt)
      'GraphValueFont'      = ("&imgfont", &small.pt) /* Axis tick values, legend entries */
      'GraphDataFont'       = ("&imgfont", &small.pt) /* Contour labels */
      'GraphUnicodeFont'    = ("&imgfont", &small.pt)
      'GraphAnnoFont'       = ("&imgfont", &small.pt);

      class color_list /
      'BgH'                 = white /* Header background */
      'BgT'                 = white /* Table background */
      'BgD'                 = white /* Data cell background */
      'Foreground'          = black /* Texts */
      'Background'          = white /* Page background */
      'Link'                = charcoal;

      class table /
      frame                 = hsides /* Outer borders: void, box, above/below, vsides/hsides, lhs/rhs */
      rules                 = groups /* Inner borders: none, all, cols, rows, groups */
      cellpadding           = 1pt /* Space between table cell contents and cell borders */
      cellspacing           = 0pt /* Space between table cells */
      borderwidth           = 1pt
      borderstyle           = solid
      outputwidth           = 100%;

      class tableheadercontainer /
      bordertopstyle        = hidden
      borderbottomstyle     = solid;

      class tablefootercontainer /
      bordertopstyle        = solid
      borderbottomstyle     = hidden;

      class body /
      bottommargin          = 1in
      topmargin             = 1in
      rightmargin           = 1in
      leftmargin            = 1in;

      class systemtitle /
      just                  = left;

      class systemfooter /
      just                  = left
      font                  = fonts('FootFont');

      class usertext /
      outputwidth           = 100%;

      class pageno /
      font                  = fonts('FootFont');

      class graphdata1 / 'LineStyle' = 1;
      class graphdata2 / 'LineStyle' = 2;
      class graphdata3 / 'LineStyle' = 3;
      class graphdata4 / 'LineStyle' = 4;
      class graphdata5 / 'LineStyle' = 5;
      class graphdata6 / 'LineStyle' = 6;
    end;

    define style styles.kenyxls;
      parent = styles.statistical;
      class fonts /
      'TitleFont'           = ("&frmfont", &normalsize.pt)
      'HeadingFont'         = ("&frmfont", &small.pt, bold)
      'DocFont'             = ("&frmfont", &small.pt)
      'FootFont'            = ("&frmfont", &tiny.pt);

      class color_list /
      'BgH'                 = black
      'FgH'                 = white
      'Foreground'          = black
      'Background'          = white;
    end;

    define style styles.kenyplot;
      parent = styles.statistical;
      class graphfonts /
      'GraphTitleFont'      = ("&imgfont", &huge.pt, bold) /* All titles */
      'GraphTitle1Font'     = ("&imgfont", &huge.pt)
      'GraphFootnoteFont'   = ("&imgfont", &large.pt, italic) /* All footnotes */
      'GraphLabelFont'      = ("&imgfont", &large.pt) /* Axis labels, legend titles */
      'GraphLabel2Font'     = ("&imgfont", &large.pt)
      'GraphValueFont'      = ("&imgfont", &small.pt) /* Axis tick values, legend entries */
      'GraphDataFont'       = ("&imgfont", &small.pt) /* Contour labels */
      'GraphUnicodeFont'    = ("&imgfont", &small.pt)
      'GraphAnnoFont'       = ("&imgfont", &small.pt);

      class graphdata1 / 'LineStyle' = 1;
      class graphdata2 / 'LineStyle' = 2;
      class graphdata3 / 'LineStyle' = 3;
      class graphdata4 / 'LineStyle' = 4;
      class graphdata5 / 'LineStyle' = 5;
      class graphdata6 / 'LineStyle' = 6;
    end;
  run;
%mend;

%init_style();
