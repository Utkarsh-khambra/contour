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

#include <terminal/Cell.h>
#include <terminal/Charset.h>
#include <terminal/ColorPalette.h>
#include <terminal/GraphicsAttributes.h>
#include <terminal/Grid.h>
#include <terminal/Hyperlink.h>
#include <terminal/Parser.h>
#include <terminal/ScreenEvents.h> // ScreenType
#include <terminal/Sequencer.h>
#include <terminal/primitives.h>

#include <fmt/format.h>

#include <bitset>
#include <functional>
#include <memory>
#include <stack>
#include <vector>

namespace terminal
{

template <typename EventListener>
class Screen;

// {{{ Modes
/// API for setting/querying terminal modes.
///
/// This abstracts away the actual implementation for more intuitive use and easier future adaptability.
class Modes
{
  public:
    void set(AnsiMode _mode, bool _enabled) { ansi_.set(static_cast<size_t>(_mode), _enabled); }

    void set(DECMode _mode, bool _enabled) { dec_.set(static_cast<size_t>(_mode), _enabled); }

    bool enabled(AnsiMode _mode) const noexcept { return ansi_.test(static_cast<size_t>(_mode)); }

    bool enabled(DECMode _mode) const noexcept { return dec_.test(static_cast<size_t>(_mode)); }

    void save(std::vector<DECMode> const& _modes)
    {
        for (DECMode const mode: _modes)
            savedModes_[mode].push_back(enabled(mode));
    }

    void restore(std::vector<DECMode> const& _modes)
    {
        for (DECMode const mode: _modes)
        {
            if (auto i = savedModes_.find(mode); i != savedModes_.end() && !i->second.empty())
            {
                auto& saved = i->second;
                set(mode, saved.back());
                saved.pop_back();
            }
        }
    }

  private:
    // TODO: make this a vector<bool> by casting from Mode, but that requires ensured small linearity in Mode
    // enum values.
    std::bitset<32> ansi_;                            // AnsiMode
    std::bitset<8452 + 1> dec_;                       // DECMode
    std::map<DECMode, std::vector<bool>> savedModes_; //!< saved DEC modes
};
// }}}

// {{{ Cursor
/// Terminal cursor data structure.
///
/// NB: Take care what to store here, as DECSC/DECRC will save/restore this struct.
struct Cursor
{
    CellLocation position { LineOffset(0), ColumnOffset(0) };
    bool autoWrap = true; // false;
    bool originMode = false;
    bool visible = true;
    GraphicsAttributes graphicsRendition {};
    CharsetMapping charsets {};
    HyperlinkId hyperlink {};
    // TODO: selective erase attribute
    // TODO: SS2/SS3 states
    // TODO: CharacterSet for GL and GR
};
// }}}

template <typename EventListener>
struct TerminalState
{
    TerminalState(Screen<EventListener>& _screen,
                  EventListener& _eventListener,
                  PageSize _pageSize,
                  LineCount _maxHistoryLineCount,
                  ImageSize _maxImageSize,
                  int _maxImageColorRegisters,
                  bool _sixelCursorConformance,
                  ColorPalette const& _colorPalette,
                  bool _allowReflowOnResize):
        eventListener { _eventListener },
        sequencer { _screen, _maxImageSize, colorPalette.defaultBackground, imageColorPalette },
        parser { std::ref(sequencer) },
        defaultColorPalette { _colorPalette },
        colorPalette { _colorPalette },
        maxImageColorRegisters { _maxImageColorRegisters },
        maxImageSize { _maxImageSize },
        maxImageSizeLimit { _maxImageSize },
        imageColorPalette { std::make_shared<SixelColorPalette>(maxImageColorRegisters,
                                                                maxImageColorRegisters) },
        imagePool { [this](Image const* _image) {
            eventListener.discardImage(*_image);
        } },
        pageSize { _pageSize },
        sixelCursorConformance { _sixelCursorConformance },
        margin { Margin::Vertical { {}, pageSize.lines.as<LineOffset>() - LineOffset(1) },
                 Margin::Horizontal { {}, pageSize.columns.as<ColumnOffset>() - ColumnOffset(1) } },
        allowReflowOnResize { _allowReflowOnResize },
        grids { Grid<Cell>(_pageSize, _allowReflowOnResize, _maxHistoryLineCount),
                Grid<Cell>(_pageSize, false, LineCount(0)) },
        activeGrid { &grids[0] },
        cursor {},
        lastCursorPosition {},
        hyperlinks { HyperlinkCache { 1024 } },
        respondToTCapQuery { true }
    {
    }

    EventListener& eventListener;
    Sequencer<EventListener> sequencer;
    parser::Parser<Sequencer<EventListener>> parser;
    int64_t instructionCounter = 0;

    ImageSize cellPixelSize; ///< contains the pixel size of a single cell, or area(cellPixelSize_) == 0 if
                             ///< unknown.

    bool focused = true;

    VTType terminalId = VTType::VT525;

    Modes modes;
    std::map<DECMode, std::vector<bool>> savedModes; //!< saved DEC modes
    ColorPalette defaultColorPalette;
    ColorPalette colorPalette;

    int maxImageColorRegisters;
    ImageSize maxImageSize;
    ImageSize maxImageSizeLimit;
    std::shared_ptr<SixelColorPalette> imageColorPalette;
    ImagePool imagePool;

    bool sixelCursorConformance = true;

    Margin margin;
    ColumnCount tabWidth { 8 };
    std::vector<ColumnOffset> tabs;

    bool allowReflowOnResize;

    ScreenType screenType = ScreenType::Main;
    std::array<Grid<Cell>, 2> grids;
    Grid<Cell>* activeGrid;

    // cursor related
    //
    Cursor cursor;
    Cursor savedCursor;
    Cursor savedPrimaryCursor; //!< saved cursor of primary-screen when switching to alt-screen.
    CellLocation lastCursorPosition;
    bool wrapPending = false;

    CursorDisplay cursorDisplay = CursorDisplay::Steady;
    CursorShape cursorShape = CursorShape::Block;

    std::string currentWorkingDirectory = {};

    // Hyperlink related
    //

    HyperlinkStorage hyperlinks {};

    // experimental features
    //
    bool respondToTCapQuery = true;

    PageSize pageSize;
    std::string windowTitle {};
    std::stack<std::string> savedWindowTitles {};
};

} // namespace terminal
