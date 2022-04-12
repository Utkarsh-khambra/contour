#include <terminal/Grid.h>
#include <terminal/ViMode.h>

#include <algorithm>

#include "fmt/core.h"

ViMode::ViMode(terminal::Grid<terminal::Cell>& _grid): grid_(_grid)
{
}
void ViMode::toggle()
{
    viModeActive = !viModeActive;
    // drawViStatusLine(toggleState_);
}

bool ViMode::searchText(std::string_view text)
{
    auto lines = grid_.logicalLines();
    for (const auto& line: lines)
    {
        auto str = line.text();
        // fmt::print("{}\n", str);
        fmt::print("Hello\n");
        auto i =
            std::search(str.begin(), str.end(), std::boyer_moore_horspool_searcher(text.begin(), text.end()));
        if (i != str.end())
            fmt::print("{}", std::string_view(&*i, text.size()));
    }
    return true;
}
