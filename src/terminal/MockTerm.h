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

#include <terminal/Screen.h>
#include <terminal/primitives.h>

namespace terminal
{

class MockTerm: public MockScreenEvents
{
  public:
    explicit MockTerm(PageSize _size, LineCount _hist = {});

    PageSize pageSize() const noexcept { return state_.pageSize; }

    Screen<MockTerm>& screen() noexcept { return primaryScreen_; }
    TerminalState<MockTerm>& state() noexcept { return state_; }
    TerminalState<MockTerm> const& state() const noexcept { return state_; }

    TerminalState<MockTerm> state_;
    Screen<MockTerm> primaryScreen_;
};

} // namespace terminal