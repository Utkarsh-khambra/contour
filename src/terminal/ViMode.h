#pragma once
#include <terminal/Cell.h>
#include <terminal/Grid.h>

#include <string_view>

class ViMode
{
  public:
    ViMode(terminal::Grid<terminal::Cell>& _grid);
    void toggle();
    bool searchText(std::string_view needle);

  private:
    bool viModeActive = false;
    terminal::Grid<terminal::Cell>& grid_;
};
