/**
 * This file is part of the "libterminal" project
 *   Copyright (c) 2019-2020 Christian Parpart <christian@parpart.family>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#pragma once

#include <terminal/Capabilities.h>
#include <terminal/TerminalState.h>
#include <terminal/Cell.h>
#include <terminal/Charset.h>
#include <terminal/Color.h>
#include <terminal/Grid.h>
#include <terminal/Hyperlink.h>
#include <terminal/Image.h>
#include <terminal/Parser.h>
#include <terminal/ScreenEvents.h>
#include <terminal/VTType.h>

#include <crispy/StrongLRUCache.h>
#include <crispy/algorithm.h>
#include <crispy/logstore.h>
#include <crispy/size.h>
#include <crispy/span.h>
#include <crispy/utils.h>

#include <unicode/grapheme_segmenter.h>
#include <unicode/width.h>

#include <fmt/format.h>

#include <algorithm>
#include <bitset>
#include <deque>
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <stack>
#include <string>
#include <string_view>
#include <vector>

template <typename T, typename std::enable_if<crispy::is_boxed<T>, bool>::type = true>
struct BoxedHasher // TODO(pr) rm
{
    crispy::StrongHash operator()(T value) const noexcept
    {
        return crispy::StrongHasher<typename T::inner_type> {}(*value);
    }
};

namespace terminal
{

/**
 * Terminal Screen.
 *
 * Implements the all VT command types and applies all instruction
 * to an internal screen buffer, maintaining width, height, and history,
 * allowing the object owner to control which part of the screen (or history)
 * to be viewn.
 */
template <typename EventListener>
class Screen: public capabilities::StaticDatabase // TODO(pr) rename to ScreenBuffer
{
  public:
    /**
     * Initializes the screen with the given screen size and callbaks.
     *
     * @param _pageSize screen dimensions in number of characters per line and number of lines.
     * @param _eventListener Interface to some VT sequence related callbacks.
     * @param _logRaw whether or not to log raw VT sequences.
     * @param _logTrace whether or not to log VT sequences in trace mode.
     * @param _maxHistoryLineCount number of lines the history must not exceed.
     */
    Screen(
        // TerminalState<EventListener> _sharedState,
        PageSize _pageSize,
        EventListener& _eventListener,
        LineCount _maxHistoryLineCount = LineCount(0),
        ImageSize _maxImageSize = ImageSize { Width(800), Height(600) },
        int _maxImageColorRegisters = 256,
        bool _sixelCursorConformance = true,
        ColorPalette _colorPalette = {},
        bool _reflowOnResize = true);

    using StaticDatabase::numericCapability;
    unsigned numericCapability(capabilities::Code _cap) const override;

    void setMaxImageColorRegisters(unsigned value) noexcept
    {
        state_.sequencer.setMaxImageColorRegisters(value);
    }

    void setSixelCursorConformance(bool _value) noexcept { state_.sixelCursorConformance = _value; }

    void setRespondToTCapQuery(bool _enable) { state_.respondToTCapQuery = _enable; }

    constexpr ImageSize cellPixelSize() const noexcept { return state_.cellPixelSize; }

    constexpr void setCellPixelSize(ImageSize _cellPixelSize) { state_.cellPixelSize = _cellPixelSize; }

    void setTerminalId(VTType _id) noexcept { state_.terminalId = _id; }

    void setMaxHistoryLineCount(LineCount _maxHistoryLineCount);
    LineCount maxHistoryLineCount() const noexcept { return grid().maxHistoryLineCount(); }

    LineCount historyLineCount() const noexcept { return grid().historyLineCount(); }

    /// Writes given data into the screen.
    void write(std::string_view _data);
    void write(std::u32string_view _data);

    void writeText(char32_t _char);
    void writeText(std::string_view _chars);

    /// Renders the full screen by passing every grid cell to the callback.
    template <typename Renderer>
    void render(Renderer&& _render, ScrollOffset _scrollOffset = {}) const
    {
        state_.activeGrid->render(std::forward<Renderer>(_render), _scrollOffset);
    }

    /// Renders the full screen as text into the given string. Each line will be terminated by LF.
    std::string renderMainPageText() const;

    /// Takes a screenshot by outputting VT sequences needed to render the current state of the screen.
    ///
    /// @note Only the screenshot of the current buffer is taken, not both (main and alternate).
    ///
    /// @returns necessary commands needed to draw the current screen state,
    ///          including initial clear screen, and initial cursor hide.
    std::string screenshot(std::function<std::string(LineOffset)> const& _postLine = {}) const;

    void setFocus(bool focused) { state_.focused = focused; }
    bool focused() const noexcept { return state_.focused; }

    // {{{ VT API
    void linefeed(); // LF

    void clearToBeginOfLine();
    void clearToEndOfLine();
    void clearLine();

    void clearToBeginOfScreen();
    void clearToEndOfScreen();
    void clearScreen();

    void clearScrollbackBuffer();

    void eraseCharacters(ColumnCount _n);  // ECH
    void insertCharacters(ColumnCount _n); // ICH
    void deleteCharacters(ColumnCount _n); // DCH
    void deleteColumns(ColumnCount _n);    // DECDC
    void insertLines(LineCount _n);        // IL
    void insertColumns(ColumnCount _n);    // DECIC

    void copyArea(Rect sourceArea, int page, CellLocation targetTopLeft, int targetPage);

    void eraseArea(int _top, int _left, int _bottom, int _right);

    void fillArea(char32_t _ch, int _top, int _left, int _bottom, int _right);

    void deleteLines(LineCount _n); // DL

    void backIndex();    // DECBI
    void forwardIndex(); // DECFI

    void moveCursorBackward(ColumnCount _n);  // CUB
    void moveCursorDown(LineCount _n);        // CUD
    void moveCursorForward(ColumnCount _n);   // CUF
    void moveCursorToBeginOfLine();           // CR
    void moveCursorToColumn(ColumnOffset _n); // CHA
    void moveCursorToLine(LineOffset _n);     // VPA
    void moveCursorToNextLine(LineCount _n);  // CNL
    void moveCursorToNextTab();               // HT
    void moveCursorToPrevLine(LineCount _n);  // CPL
    void moveCursorUp(LineCount _n);          // CUU

    void cursorBackwardTab(TabStopCount _n);            // CBT
    void cursorForwardTab(TabStopCount _n);             // CHT
    void backspace();                                   // BS
    void horizontalTabClear(HorizontalTabClear _which); // TBC
    void horizontalTabSet();                            // HTS

    void index();        // IND
    void reverseIndex(); // RI

    void setMark();
    void deviceStatusReport();           // DSR
    void reportCursorPosition();         // CPR
    void reportExtendedCursorPosition(); // DECXCPR
    void selectConformanceLevel(VTType _level);
    void requestDynamicColor(DynamicColorName _name);
    void requestCapability(capabilities::Code _code);
    void requestCapability(std::string_view _name);
    void sendDeviceAttributes();
    void sendTerminalId();

    /// Sets the current working directory as file:// URL.
    void setCurrentWorkingDirectory(std::string const& _url); // OSC 7

    /// @returns either an empty string or a file:// URL of the last set working directory.
    std::string const& currentWorkingDirectory() const noexcept { return state_.currentWorkingDirectory; }

    void hyperlink(std::string _id, std::string _uri);                   // OSC 8
    void notify(std::string const& _title, std::string const& _content); // OSC 777

    void captureBuffer(int _numLines, bool _logicalLines);

    void setForegroundColor(Color _color);
    void setBackgroundColor(Color _color);
    void setUnderlineColor(Color _color);
    void setCursorStyle(CursorDisplay _display, CursorShape _shape);
    void setGraphicsRendition(GraphicsRendition _rendition);
    void setTopBottomMargin(std::optional<LineOffset> _top, std::optional<LineOffset> _bottom);
    void setLeftRightMargin(std::optional<ColumnOffset> _left, std::optional<ColumnOffset> _right);
    void screenAlignmentPattern();
    void sendMouseEvents(MouseProtocol _protocol, bool _enable);
    void applicationKeypadMode(bool _enable);
    void designateCharset(CharsetTable _table, CharsetId _charset);
    void singleShiftSelect(CharsetTable _table);
    void requestPixelSize(RequestPixelSize _area);
    void requestCharacterSize(RequestPixelSize _area);
    void sixelImage(ImageSize _pixelSize, Image::Data&& _rgba);
    void requestStatusString(RequestStatusString _value);
    void requestTabStops();
    void resetDynamicColor(DynamicColorName _name);
    void setDynamicColor(DynamicColorName _name, RGBColor _color);
    void inspect();
    void smGraphics(XtSmGraphics::Item _item, XtSmGraphics::Action _action, XtSmGraphics::Value _value);
    // }}}

    void setMaxImageSize(ImageSize _effective, ImageSize _limit)
    {
        state_.maxImageSize = _effective;
        state_.maxImageSizeLimit = _limit;
    }

    ImageSize maxImageSize() const noexcept { return state_.maxImageSize; }
    ImageSize maxImageSizeLimit() const noexcept { return state_.maxImageSizeLimit; }

    std::shared_ptr<Image const> uploadImage(ImageFormat _format,
                                             ImageSize _imageSize,
                                             Image::Data&& _pixmap);

    /**
     * Renders an image onto the screen.
     *
     * @p _imageId ID to the image to be rendered.
     * @p _topLeft Screen coordinate to start rendering the top/left corner of the image.
     * @p _gridSize Screen grid size to span the image into.
     * @p _imageOffset Offset into the image in screen grid coordinate to start rendering from.
     * @p _imageSize Size of the full image in Screen grid coordinates.
     * @p _alignmentPolicy render the image using the given image alignment policy.
     * @p _resizePolicy render the image using the given image resize policy.
     * @p _autoScroll Boolean indicating whether or not the screen should scroll if the image cannot be fully
     * displayed otherwise.
     */
    void renderImage(std::shared_ptr<Image const> _image,
                     CellLocation _topLeft,
                     GridSize _gridSize,
                     CellLocation _imageOffset,
                     ImageSize _imageSize,
                     ImageAlignment _alignmentPolicy,
                     ImageResize _resizePolicy,
                     bool _autoScroll);

    void inspect(std::string const& _message, std::ostream& _os) const;

    // reset screen
    void resetSoft();
    void resetHard();

    // for DECSC and DECRC
    void setMode(AnsiMode _mode, bool _enabled);
    void setMode(DECMode _mode, bool _enabled);
    void saveCursor();
    void restoreCursor();
    void restoreCursor(Cursor const& _savedCursor);
    void saveModes(std::vector<DECMode> const& _modes);
    void restoreModes(std::vector<DECMode> const& _modes);
    void requestAnsiMode(int _mode);
    void requestDECMode(int _mode);

    PageSize pageSize() const noexcept { return state_.pageSize; }
    void resize(PageSize _newSize);

    /// Implements semantics for  DECCOLM / DECSCPP.
    void resizeColumns(ColumnCount _newColumnCount, bool _clear);

    bool isCursorInsideMargins() const noexcept
    {
        bool const insideVerticalMargin = state_.margin.vertical.contains(state_.cursor.position.line);
        bool const insideHorizontalMargin =
            !isModeEnabled(DECMode::LeftRightMargin)
            || state_.margin.horizontal.contains(state_.cursor.position.column);
        return insideVerticalMargin && insideHorizontalMargin;
    }

    constexpr CellLocation realCursorPosition() const noexcept { return state_.cursor.position; }

    constexpr CellLocation logicalCursorPosition() const noexcept
    {
        if (!state_.cursor.originMode)
            return realCursorPosition();
        else
            return CellLocation { state_.cursor.position.line - state_.margin.vertical.from,
                                  state_.cursor.position.column - state_.margin.horizontal.from };
    }

    constexpr CellLocation origin() const noexcept
    {
        if (!state_.cursor.originMode)
            return {};

        return { state_.margin.vertical.from, state_.margin.horizontal.from };
    }

    Cursor const& cursor() const noexcept { return state_.cursor; }

    /// Returns identity if DECOM is disabled (default), but returns translated coordinates if DECOM is
    /// enabled.
    CellLocation toRealCoordinate(CellLocation pos) const noexcept
    {
        if (!state_.cursor.originMode)
            return pos;
        else
            return { pos.line + state_.margin.vertical.from, pos.column + state_.margin.horizontal.from };
    }

    /// Clamps given coordinates, respecting DECOM (Origin Mode).
    CellLocation clampCoordinate(CellLocation coord) const noexcept
    {
        if (state_.cursor.originMode)
            return clampToOrigin(coord);
        else
            return clampToScreen(coord);
    }

    /// Clamps given logical coordinates to margins as used in when DECOM (origin mode) is enabled.
    CellLocation clampToOrigin(CellLocation coord) const noexcept
    {
        return { std::clamp(coord.line, LineOffset { 0 }, state_.margin.vertical.to),
                 std::clamp(coord.column, ColumnOffset { 0 }, state_.margin.horizontal.to) };
    }

    LineOffset clampedLine(LineOffset _line) const noexcept
    {
        return std::clamp(_line, LineOffset(0), boxed_cast<LineOffset>(state_.pageSize.lines) - 1);
    }

    ColumnOffset clampedColumn(ColumnOffset _column) const noexcept
    {
        return std::clamp(_column, ColumnOffset(0), boxed_cast<ColumnOffset>(state_.pageSize.columns) - 1);
    }

    CellLocation clampToScreen(CellLocation coord) const noexcept
    {
        return { clampedLine(coord.line), clampedColumn(coord.column) };
    }

    // Tests if given coordinate is within the visible screen area.
    constexpr bool contains(CellLocation _coord) const noexcept
    {
        return LineOffset(0) <= _coord.line && _coord.line < boxed_cast<LineOffset>(state_.pageSize.lines)
               && ColumnOffset(0) <= _coord.column
               && _coord.column <= boxed_cast<ColumnOffset>(state_.pageSize.columns);
    }

    Cell& usePreviousCell() noexcept
    {
        return useCellAt(state_.lastCursorPosition.line, state_.lastCursorPosition.column);
    }

    Line<Cell>& currentLine() { return grid().lineAt(state_.cursor.position.line); }
    Line<Cell> const& currentLine() const { return grid().lineAt(state_.cursor.position.line); }

    Cell& useCurrentCell() noexcept { return useCellAt(state_.cursor.position); }
    Cell const& currentCell() const noexcept { return at(state_.cursor.position); }

    void moveCursorTo(LineOffset _line, ColumnOffset _column);

    /// Gets a reference to the cell relative to screen origin (top left, 1:1).
    Cell& at(LineOffset _line, ColumnOffset _column) noexcept { return grid().useCellAt(_line, _column); }
    Cell& useCellAt(LineOffset _line, ColumnOffset _column) noexcept
    {
        return grid().lineAt(_line).useCellAt(_column);
    }

    /// Gets a reference to the cell relative to screen origin (top left, 1:1).
    Cell const& at(LineOffset _line, ColumnOffset _column) const noexcept
    {
        return grid().at(_line, _column);
    }

    Cell& at(CellLocation p) noexcept { return useCellAt(p.line, p.column); }
    Cell& useCellAt(CellLocation p) noexcept { return useCellAt(p.line, p.column); }
    Cell const& at(CellLocation p) const noexcept { return grid().at(p.line, p.column); }

    bool isPrimaryScreen() const noexcept { return state_.activeGrid == &state_.grids[0]; }
    bool isAlternateScreen() const noexcept { return state_.activeGrid == &state_.grids[1]; }

    bool isModeEnabled(AnsiMode m) const noexcept { return state_.modes.enabled(m); }
    bool isModeEnabled(DECMode m) const noexcept { return state_.modes.enabled(m); }

    bool isModeEnabled(std::variant<AnsiMode, DECMode> m) const
    {
        if (std::holds_alternative<AnsiMode>(m))
            return state_.modes.enabled(std::get<AnsiMode>(m));
        else
            return state_.modes.enabled(std::get<DECMode>(m));
    }

    bool verticalMarginsEnabled() const noexcept { return isModeEnabled(DECMode::Origin); }
    bool horizontalMarginsEnabled() const noexcept { return isModeEnabled(DECMode::LeftRightMargin); }

    Margin margin() const noexcept { return state_.margin; }

    void setTabWidth(ColumnCount _value) { state_.tabWidth = _value; }

    std::string const& windowTitle() const noexcept { return state_.windowTitle; }

    /// Finds the next marker right after the given line position.
    ///
    /// @paramn _currentCursorLine the line number of the current cursor (1..N) for screen area, or
    ///                            (0..-N) for savedLines area
    /// @return cursor position relative to screen origin (1, 1), that is, if line Number os >= 1, it's
    ///         in the screen area, and in the savedLines area otherwise.
    std::optional<LineOffset> findMarkerDownwards(LineOffset _currentCursorLine) const;

    /// Finds the previous marker right next to the given line position.
    ///
    /// @paramn _currentCursorLine the line number of the current cursor (1..N) for screen area, or
    ///                            (0..-N) for savedLines area
    /// @return cursor position relative to screen origin (1, 1), that is, if line Number os >= 1, it's
    ///         in the screen area, and in the savedLines area otherwise.
    std::optional<LineOffset> findMarkerUpwards(LineOffset _currentCursorLine) const;

    /// ScreenBuffer's type, such as main screen or alternate screen.
    ScreenType bufferType() const noexcept { return state_.screenType; }

    bool synchronizeOutput() const noexcept { return false; } // TODO

    EventListener& eventListener() noexcept { return state_.eventListener; }
    EventListener const& eventListener() const noexcept { return state_.eventListener; }

    void setWindowTitle(std::string const& _title);
    void saveWindowTitle();
    void restoreWindowTitle();

    void setMaxImageSize(ImageSize _size) noexcept { state_.sequencer.setMaxImageSize(_size); }

    void scrollUp(LineCount n) { scrollUp(n, state_.margin); }
    void scrollDown(LineCount n) { scrollDown(n, state_.margin); }

    void verifyState() const;

    // interactive replies
    void reply(std::string const& message) { state_.eventListener.reply(message); }

    template <typename... T>
    void reply(fmt::format_string<T...> fmt, T&&... args)
    {
        reply(fmt::vformat(fmt, fmt::make_format_args(args...)));
    }

    /// @returns the primary screen's grid.
    Grid<Cell>& primaryGrid() noexcept { return state_.grids[0]; }

    /// @returns the alternate  screen's grid.
    Grid<Cell>& alternateGrid() noexcept { return state_.grids[1]; }

    /// @returns the primary screen's grid if primary screen is active.
    Grid<Cell> const& grid() const noexcept { return *state_.activeGrid; }

    /// @returns the primary screen's grid if primary screen is active.
    Grid<Cell>& grid() noexcept { return *state_.activeGrid; }

    /// @returns true iff given absolute line number is wrapped, false otherwise.
    bool isLineWrapped(LineOffset _lineNumber) const noexcept
    {
        return state_.activeGrid->isLineWrapped(_lineNumber);
    }

    ColorPalette& colorPalette() noexcept { return state_.colorPalette; }
    ColorPalette const& colorPalette() const noexcept { return state_.colorPalette; }

    ColorPalette& defaultColorPalette() noexcept { return state_.defaultColorPalette; }
    ColorPalette const& defaultColorPalette() const noexcept { return state_.defaultColorPalette; }

    std::shared_ptr<HyperlinkInfo> hyperlinkAt(CellLocation pos) noexcept
    {
        return state_.hyperlinks.hyperlinkById(at(pos).hyperlink());
    }

    HyperlinkStorage const& hyperlinks() const noexcept { return state_.hyperlinks; }

  private:
    void setBuffer(ScreenType _type);
    void applyPageSizeToCurrentBuffer();

    void clearAllTabs();
    void clearTabUnderCursor();
    void setTabUnderCursor();

    /// Applies LF but also moves cursor to given column @p _column.
    void linefeed(ColumnOffset _column);

    void writeCharToCurrentAndAdvance(char32_t _codepoint) noexcept;
    void clearAndAdvance(int _offset) noexcept;

    void fail(std::string const& _message) const;

    void scrollUp(LineCount n, GraphicsAttributes sgr, Margin margin);
    void scrollUp(LineCount n, Margin margin);
    void scrollDown(LineCount n, Margin margin);
    void insertChars(LineOffset lineNo, ColumnCount _n);
    void deleteChars(LineOffset lineNo, ColumnOffset _column, ColumnCount _count);

    /// Sets the current column to given logical column number.
    void setCurrentColumn(ColumnOffset _n);

    EventListener& eventListener_;
    TerminalState<EventListener> state_;
};

} // namespace terminal

namespace fmt // {{{
{
template <>
struct formatter<terminal::Margin::Horizontal>
{
    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx)
    {
        return ctx.begin();
    }
    template <typename FormatContext>
    auto format(const terminal::Margin::Horizontal range, FormatContext& ctx)
    {
        return format_to(ctx.out(), "{}..{}", range.from, range.to);
    }
};

template <>
struct formatter<terminal::Margin::Vertical>
{
    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx)
    {
        return ctx.begin();
    }
    template <typename FormatContext>
    auto format(const terminal::Margin::Vertical range, FormatContext& ctx)
    {
        return format_to(ctx.out(), "{}..{}", range.from, range.to);
    }
};

template <>
struct formatter<terminal::Cursor>
{
    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx)
    {
        return ctx.begin();
    }
    template <typename FormatContext>
    auto format(const terminal::Cursor cursor, FormatContext& ctx)
    {
        return format_to(ctx.out(),
                         "({}:{}{})",
                         cursor.position.line,
                         cursor.position.column,
                         cursor.visible ? "" : ", (invis)");
    }
};

template <>
struct formatter<terminal::ScreenType>
{
    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx)
    {
        return ctx.begin();
    }

    template <typename FormatContext>
    auto format(const terminal::ScreenType value, FormatContext& ctx)
    {
        switch (value)
        {
        case terminal::ScreenType::Main: return format_to(ctx.out(), "main");
        case terminal::ScreenType::Alternate: return format_to(ctx.out(), "alternate");
        }
        return format_to(ctx.out(), "({})", static_cast<unsigned>(value));
    }
};

} // namespace fmt
