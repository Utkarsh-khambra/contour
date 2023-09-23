// SPDX-License-Identifier: Apache-2.0
#pragma once

#include <vtbackend/VTType.h>

#include <crispy/defines.h>
#include <crispy/escape.h>
#include <crispy/indexed.h>
#include <crispy/sort.h>

#include <fmt/format.h>

#include <gsl/span>

#include <array>
#include <optional>
#include <string>
#include <vector>

namespace terminal
{

enum class FunctionCategory : uint8_t
{
    C0 = 0,
    ESC = 1,
    CSI = 2,
    OSC = 3,
    DCS = 4,
};

/// Defines a function with all its syntax requirements plus some additional meta information.
struct FunctionDefinition // TODO: rename Function
{
    FunctionCategory category;  // (3 bits) C0, ESC, CSI, OSC, DCS
    char leader;                // (3 bits) 0x3C..0x3F (one of: < = > ?, or 0x00 for none)
    char intermediate;          // (4 bits) 0x20..0x2F (intermediates, usually just one, or 0x00 if none)
    char finalSymbol;           // (7 bits) 0x30..0x7E (final character)
    uint8_t minimumParameters;  // (4 bits) 0..7
    uint16_t maximumParameters; // (10 bits) 0..1024 for integer value (OSC function parameter)

    // Conformance level and extension are mutually exclusive.
    // But it is unclear to me whether or not it is guaranteed to always have a constexpr-aware std::variant.
    // So keep it the classic way (for now).
    VTType conformanceLevel;
    VTExtension extension = VTExtension::None;

    std::string_view mnemonic;
    std::string_view comment;

    using id_type = uint32_t;

    [[nodiscard]] constexpr id_type id() const noexcept
    {
        // clang-format off
        unsigned constexpr CategoryShift     = 0;
        unsigned constexpr LeaderShift       = 3;
        unsigned constexpr IntermediateShift = 3 + 3;
        unsigned constexpr FinalShift        = 3 + 3 + 4;
        unsigned constexpr MinParamShift     = 3 + 3 + 4 + 7;
        unsigned constexpr MaxParamShift     = 3 + 3 + 4 + 7 + 4;
        // clang-format on

        // if (category == FunctionCategory::C0)
        //     return static_cast<id_type>(category) | finalSymbol << 3;

        auto const maskCat = static_cast<id_type>(category) << CategoryShift;

        // 0x3C..0x3F; (one of: < = > ?, or 0x00 for none)
        auto const maskLeader = !leader ? 0 : (static_cast<id_type>(leader) - 0x3C) << LeaderShift;

        // 0x20..0x2F: (intermediates, usually just one, or 0x00 if none)
        auto const maskInterm =
            !intermediate ? 0 : (static_cast<id_type>(intermediate) - 0x20 + 1) << IntermediateShift;

        // 0x40..0x7E: final character
        auto const maskFinalS = !finalSymbol ? 0 : (static_cast<id_type>(finalSymbol) - 0x40) << FinalShift;
        auto const maskMinPar = static_cast<id_type>(minimumParameters) << MinParamShift;
        auto const maskMaxPar = static_cast<id_type>(maximumParameters) << MaxParamShift;

        return maskCat | maskLeader | maskInterm | maskFinalS | maskMinPar | maskMaxPar;
    }

    constexpr operator id_type() const noexcept { return id(); }
};

constexpr int compare(FunctionDefinition const& a, FunctionDefinition const& b)
{
    if (a.category != b.category)
        return static_cast<int>(a.category) - static_cast<int>(b.category);

    if (a.finalSymbol != b.finalSymbol) // XXX
        return static_cast<int>(a.finalSymbol) - static_cast<int>(b.finalSymbol);

    if (a.leader != b.leader)
        return a.leader - b.leader;

    if (a.intermediate != b.intermediate)
        return static_cast<int>(a.intermediate) - static_cast<int>(b.intermediate);

    if (a.minimumParameters != b.minimumParameters)
        return static_cast<int>(a.minimumParameters) - static_cast<int>(b.minimumParameters);

    return static_cast<int>(a.maximumParameters) - static_cast<int>(b.maximumParameters);
}

// clang-format off
constexpr bool operator==(FunctionDefinition const& a, FunctionDefinition const& b) noexcept { return compare(a, b) == 0; }
constexpr bool operator!=(FunctionDefinition const& a, FunctionDefinition const& b) noexcept { return compare(a, b) != 0; }
constexpr bool operator<=(FunctionDefinition const& a, FunctionDefinition const& b) noexcept { return compare(a, b) <= 0; }
constexpr bool operator>=(FunctionDefinition const& a, FunctionDefinition const& b) noexcept { return compare(a, b) >= 0; }
constexpr bool operator<(FunctionDefinition const& a, FunctionDefinition const& b) noexcept { return compare(a, b) < 0; }
constexpr bool operator>(FunctionDefinition const& a, FunctionDefinition const& b) noexcept { return compare(a, b) > 0; }
// clang-format on

struct FunctionSelector
{
    /// represents the corresponding function category.
    FunctionCategory category;
    /// an optional value between 0x3C .. 0x3F
    char leader;
    /// number of arguments supplied
    int argc;
    /// an optional intermediate character between (0x20 .. 0x2F)
    char intermediate;
    /// between 0x40 .. 0x7F
    char finalSymbol;
};

constexpr int compare(FunctionSelector const& a, FunctionDefinition const& b) noexcept
{
    if (a.category != b.category)
        return static_cast<int>(a.category) - static_cast<int>(b.category);

    if (a.finalSymbol != b.finalSymbol)
        return a.finalSymbol - b.finalSymbol;

    if (a.leader != b.leader)
        return a.leader - b.leader;

    if (a.intermediate != b.intermediate)
        return a.intermediate - b.intermediate;

    if (a.category == FunctionCategory::OSC)
        return static_cast<int>(a.argc) - static_cast<int>(b.maximumParameters);

    if (a.argc < b.minimumParameters)
        return -1;

    if (a.argc > b.maximumParameters)
        return +1;

    return 0;
}

namespace detail // {{{
{
    constexpr auto C0(char finalCharacter,
                      std::string_view mnemonic,
                      std::string_view description,
                      VTType vt = VTType::VT100) noexcept
    {
        // clang-format off
        return FunctionDefinition { FunctionCategory::C0, 0, 0, finalCharacter, 0, 0, vt,
                                    VTExtension::None, mnemonic, description };
        // clang-format on
    }

    constexpr auto OSC(uint16_t code,
                       VTExtension ext,
                       std::string_view mnemonic,
                       std::string_view description) noexcept
    {
        // clang-format off
        return FunctionDefinition { FunctionCategory::OSC, 0, 0, 0, 0, code,
                                    VTType::VT100,
                                    ext,
                                    mnemonic,
                                    description };
        // clang-format on
    }

    constexpr auto ESC(std::optional<char> intermediate,
                       char finalCharacter,
                       VTType vt,
                       std::string_view mnemonic,
                       std::string_view description) noexcept
    {
        return FunctionDefinition { FunctionCategory::ESC,
                                    0,
                                    intermediate.value_or(0),
                                    finalCharacter,
                                    0,
                                    0,
                                    vt,
                                    VTExtension::None,
                                    mnemonic,
                                    description };
    }

    constexpr auto CSI(std::optional<char> leader,
                       uint8_t argc0,
                       uint8_t argc1,
                       std::optional<char> intermediate,
                       char finalCharacter,
                       VTType vt,
                       std::string_view mnemonic,
                       std::string_view description) noexcept
    {
        // TODO: static_assert on leader/intermediate range-or-null
        return FunctionDefinition { FunctionCategory::CSI,
                                    leader.value_or(0),
                                    intermediate.value_or(0),
                                    finalCharacter,
                                    argc0,
                                    argc1,
                                    vt,
                                    VTExtension::None,
                                    mnemonic,
                                    description };
    }

    constexpr auto CSI(std::optional<char> leader,
                       uint8_t argc0,
                       uint8_t argc1,
                       std::optional<char> intermediate,
                       char finalCharacter,
                       VTExtension ext,
                       std::string_view mnemonic,
                       std::string_view description) noexcept
    {
        // TODO: static_assert on leader/intermediate range-or-null
        return FunctionDefinition { FunctionCategory::CSI,
                                    leader.value_or(0),
                                    intermediate.value_or(0),
                                    finalCharacter,
                                    argc0,
                                    argc1,
                                    VTType::VT100,
                                    ext,
                                    mnemonic,
                                    description };
    }

    constexpr auto DCS(std::optional<char> leader,
                       uint8_t argc0,
                       uint8_t argc1,
                       std::optional<char> intermediate,
                       char finalCharacter,
                       VTType vt,
                       std::string_view mnemonic,
                       std::string_view description) noexcept
    {
        // TODO: static_assert on leader/intermediate range-or-null
        return FunctionDefinition { FunctionCategory::DCS,
                                    leader.value_or(0),
                                    intermediate.value_or(0),
                                    finalCharacter,
                                    argc0,
                                    argc1,
                                    vt,
                                    VTExtension::None,
                                    mnemonic,
                                    description };
    }

    constexpr auto DCS(std::optional<char> leader,
                       uint8_t argc0,
                       uint8_t argc1,
                       std::optional<char> intermediate,
                       char finalCharacter,
                       VTExtension ext,
                       std::string_view mnemonic,
                       std::string_view description) noexcept
    {
        // TODO: static_assert on leader/intermediate range-or-null
        return FunctionDefinition { FunctionCategory::DCS,
                                    leader.value_or(0),
                                    intermediate.value_or(0),
                                    finalCharacter,
                                    argc0,
                                    argc1,
                                    VTType::VT100,
                                    ext,
                                    mnemonic,
                                    description };
    }
} // namespace detail

// clang-format off

#define STRINGIZE2(x) #x
#define STRINGIZE(x) STRINGIZE2(x)
#define CONTROL_CHAR(a,name, description,...) constexpr inline auto name = detail::C0(a, STRINGIZE(name), description)

// C0
CONTROL_CHAR( '\x04',EOT , "End of Transmission");
CONTROL_CHAR( '\x07',BEL , "Bell");
CONTROL_CHAR( '\x08',BS, "Backspace");
CONTROL_CHAR( '\x09',TAB , "Tab");
CONTROL_CHAR( '\x0A',LF, "Line Feed");
CONTROL_CHAR( '\x0B',VT ,"Vertical Tab"); // Even though VT means Vertical Tab,VT  , it seems that xterm is doing an IND instead.
CONTROL_CHAR( '\x0C',FF ,"Form Feed");
CONTROL_CHAR( '\x0D',CR ,"Carriage Return");
CONTROL_CHAR( '\x0E',LS1 , "Shift Out; Maps G1 into GL.", VTType::VT220);
CONTROL_CHAR( '\x0F',LS0 , "Shift In; Maps G0 into GL (the default).", VTType::VT220);
#undef CONTROL_CHAR

// SCS to support (G0, G1, G2, G3)
// A        UK (British), VT100
// B        USASCII, VT100
// 4        Dutch, VT200
// C
// S        Finnish, VT200
// R
// f        French, VT200
// Q
// 9        French Canadian, VT200
// K        VT200
// " >      Greek VT500
// % =      Hebrew VT500
// Y        Italian, VT200
// `
// E
// 6        Norwegian/Danish, VT200
// % 6      Portuguese, VT300
// Z        Spanish, VT200.
// H
// 7        Swedish, VT200.
// =        Swiss, VT200.
// % 2      Turkish, VT500.

#define ESC_FUNCTION(name, a, arg, vttype,description ) constexpr inline auto name  = detail::ESC(a, arg, vttype, STRINGIZE(name), description)


// ESC functions
ESC_FUNCTION(SCS_G0_SPECIAL ,'(', '0', VTType::VT100, "Set G0 to DEC Special Character and Line Drawing Set");
ESC_FUNCTION(SCS_G0_USASCII ,'(', 'B', VTType::VT100, "Set G0 to USASCII");
ESC_FUNCTION(SCS_G1_SPECIAL ,')', '0', VTType::VT100, "Set G1 to DEC Special Character and Line Drawing Set");
ESC_FUNCTION(SCS_G1_USASCII ,')', 'B', VTType::VT100, "Set G1 to USASCII");
ESC_FUNCTION(DECALN, '#', '8', VTType::VT100, "Screen Alignment Pattern");
ESC_FUNCTION(DECBI,std::nullopt, '6', VTType::VT100, "Back Index");
ESC_FUNCTION(DECFI,std::nullopt, '9', VTType::VT100, "Forward Index");
ESC_FUNCTION(DECKPAM,std::nullopt, '=', VTType::VT100, "Keypad Application Mode");
ESC_FUNCTION(DECKPNM ,std::nullopt, '>', VTType::VT100, "Keypad Numeric Mode");
ESC_FUNCTION(DECRS   ,std::nullopt, '8', VTType::VT100, "Restore Cursor");
ESC_FUNCTION(DECSC   ,std::nullopt, '7', VTType::VT100, "Save Cursor");
ESC_FUNCTION(HTS     ,std::nullopt, 'H', VTType::VT100, "Horizontal Tab Set");
ESC_FUNCTION(IND     ,std::nullopt, 'D', VTType::VT100, "Index");
ESC_FUNCTION(NEL     ,std::nullopt, 'E', VTType::VT100, "Next Line");
ESC_FUNCTION(RI      ,std::nullopt, 'M', VTType::VT100, "Reverse Index");
ESC_FUNCTION(RIS     ,std::nullopt, 'c', VTType::VT100, "Reset to Initial State (Hard Reset)");
ESC_FUNCTION(SS2     ,std::nullopt, 'N', VTType::VT220, "Single Shift Select (G2 Character Set)");
ESC_FUNCTION(SS3     ,std::nullopt, 'O', VTType::VT220, "Single Shift Select (G3 Character Set)");

#undef ESC_FUNCTION
// constexpr inline auto SCS_G0_SPECIAL = detail::ESC('(', '0', VTType::VT100, "SCS_G0_SPECIAL", "Set G0 to DEC Special Character and Line Drawing Set");// NOLINT
// constexpr inline auto SCS_G0_USASCII = detail::ESC('(', 'B', VTType::VT100, "SCS_G0_USASCII", "Set G0 to USASCII");// NOLINT
// constexpr inline auto SCS_G1_SPECIAL = detail::ESC(')', '0', VTType::VT100, "SCS_G1_SPECIAL", "Set G1 to DEC Special Character and Line Drawing Set");// NOLINT
// constexpr inline auto SCS_G1_USASCII = detail::ESC(')', 'B', VTType::VT100, "SCS_G1_USASCII", "Set G1 to USASCII");//NOLINT
// constexpr inline auto DECALN  = detail::ESC('#', '8', VTType::VT100, "DECALN", "Screen Alignment Pattern");
// constexpr inline auto DECBI   = detail::ESC(std::nullopt, '6', VTType::VT100, "DECBI", "Back Index");
// constexpr inline auto DECFI   = detail::ESC(std::nullopt, '9', VTType::VT100, "DECFI", "Forward Index");
// constexpr inline auto DECKPAM = detail::ESC(std::nullopt, '=', VTType::VT100, "DECKPAM", "Keypad Application Mode");
// constexpr inline auto DECKPNM = detail::ESC(std::nullopt, '>', VTType::VT100, "DECKPNM", "Keypad Numeric Mode");
// constexpr inline auto DECRS   = detail::ESC(std::nullopt, '8', VTType::VT100, "DECRS", "Restore Cursor");
// constexpr inline auto DECSC   = detail::ESC(std::nullopt, '7', VTType::VT100, "DECSC", "Save Cursor");
// constexpr inline auto HTS     = detail::ESC(std::nullopt, 'H', VTType::VT100, "HTS", "Horizontal Tab Set");
// constexpr inline auto IND     = detail::ESC(std::nullopt, 'D', VTType::VT100, "IND", "Index");
// constexpr inline auto NEL     = detail::ESC(std::nullopt, 'E', VTType::VT100, "NEL", "Next Line");
// constexpr inline auto RI      = detail::ESC(std::nullopt, 'M', VTType::VT100, "RI", "Reverse Index");
// constexpr inline auto RIS     = detail::ESC(std::nullopt, 'c', VTType::VT100, "RIS", "Reset to Initial State (Hard Reset)");
// constexpr inline auto SS2     = detail::ESC(std::nullopt, 'N', VTType::VT220, "SS2", "Single Shift Select (G2 Character Set)");
// constexpr inline auto SS3     = detail::ESC(std::nullopt, 'O', VTType::VT220, "SS3", "Single Shift Select (G3 Character Set)");


#define CSI_FUNCTION(name, a, argmin, argmax, intermediate, finalchar, vttype, description) constexpr inline auto name = detail::CSI(a, argmin, argmax, intermediate, finalchar, vttype, STRINGIZE(name), description)

// CSI
constexpr inline auto ArgsMax = 127; // this is the maximum number that fits into 7 bits.

// CSI functions
// constexpr inline auto ANSISYSSC   = detail::CSI(std::nullopt, 0, 0, std::nullopt, 'u', VTType::VT100, "ANSISYSSC", "Save Cursor (ANSI.SYS)");
// constexpr inline auto CBT         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'Z', VTType::VT100, "CBT", "Cursor Backward Tabulation");
// constexpr inline auto CHA         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'G', VTType::VT100, "CHA", "Move cursor to column");
// constexpr inline auto CHT         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'I', VTType::VT100, "CHT", "Cursor Horizontal Forward Tabulation");
// constexpr inline auto CNL         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'E', VTType::VT100, "CNL", "Move cursor to next line");
// constexpr inline auto CPL         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'F', VTType::VT100, "CPL", "Move cursor to previous line");
// constexpr inline auto CPR         = detail::CSI(std::nullopt, 1, 1, std::nullopt, 'n', VTType::VT100, "CPR", "Request Cursor position");
// constexpr inline auto CUB         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'D', VTType::VT100, "CUB", "Move cursor backward");
// constexpr inline auto CUD         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'B', VTType::VT100, "CUD", "Move cursor down");
// constexpr inline auto CUF         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'C', VTType::VT100, "CUF", "Move cursor forward");
// constexpr inline auto CUP         = detail::CSI(std::nullopt, 0, 2, std::nullopt, 'H', VTType::VT100, "CUP", "Move cursor to position");
// constexpr inline auto CUU         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'A', VTType::VT100, "CUU", "Move cursor up");
// constexpr inline auto DA1         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'c', VTType::VT100, "DA1", "Send primary device attributes");
// constexpr inline auto DA2         = detail::CSI('>', 0, 1, std::nullopt, 'c', VTType::VT100, "DA2", "Send secondary device attributes");
// constexpr inline auto DA3         = detail::CSI('=', 0, 1, std::nullopt, 'c', VTType::VT100, "DA3", "Send tertiary device attributes");
// constexpr inline auto DCH         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'P', VTType::VT100, "DCH", "Delete characters");
// constexpr inline auto DECCARA     = detail::CSI(std::nullopt, 5, ArgsMax, '$', 'r', VTType::VT420, "DECCARA", "Change Attributes in Rectangular Area");
// constexpr inline auto DECCRA      = detail::CSI(std::nullopt, 0, 8, '$', 'v', VTType::VT420, "DECCRA", "Copy rectangular area");
// constexpr inline auto DECERA      = detail::CSI(std::nullopt, 0, 4, '$', 'z', VTType::VT420, "DECERA", "Erase rectangular area");
// constexpr inline auto DECFRA      = detail::CSI(std::nullopt, 0, 5, '$', 'x', VTType::VT420, "DECFRA", "Fill rectangular area");
// constexpr inline auto DECDC       = detail::CSI(std::nullopt, 0, 1, '\'', '~', VTType::VT420, "DECDC", "Delete column");
// constexpr inline auto DECIC       = detail::CSI(std::nullopt, 0, 1, '\'', '}', VTType::VT420, "DECIC", "Insert column");
// constexpr inline auto DECSCA      = detail::CSI(std::nullopt, 0, 1, '"', 'q', VTType::VT240, "DECSCA", "Select Character Protection Attribute");
// constexpr inline auto DECSED      = detail::CSI('?', 0, 1, std::nullopt, 'J', VTType::VT240, "DECSED", "Selective Erase in Display");
// constexpr inline auto DECSERA     = detail::CSI(std::nullopt, 0, 4, '$', '{', VTType::VT240, "DECSERA", "Selective Erase in Rectangular Area");
// constexpr inline auto DECSEL      = detail::CSI('?', 0, 1, std::nullopt, 'K', VTType::VT240, "DECSEL", "Selective Erase in Line");
// constexpr inline auto XTRESTORE   = detail::CSI('?', 0, ArgsMax, std::nullopt, 'r', VTExtension::XTerm, "XTRESTORE", "Restore DEC private modes.");
// constexpr inline auto XTSAVE      = detail::CSI('?', 0, ArgsMax, std::nullopt, 's', VTExtension::XTerm, "XTSAVE", "Save DEC private modes.");
// constexpr inline auto DECRM       = detail::CSI('?', 1, ArgsMax, std::nullopt, 'l', VTType::VT100, "DECRM", "Reset DEC-mode");
// constexpr inline auto DECRQM      = detail::CSI('?', 1, 1, '$', 'p', VTType::VT100, "DECRQM", "Request DEC-mode");
// constexpr inline auto DECRQM_ANSI = detail::CSI(std::nullopt, 1, 1, '$', 'p', VTType::VT100, "DECRQM_ANSI", "Request ANSI-mode");// NOLINT
// constexpr inline auto DECRQPSR    = detail::CSI(std::nullopt, 1, 1, '$', 'w', VTType::VT320, "DECRQPSR", "Request presentation state report");
// constexpr inline auto DECSCL      = detail::CSI(std::nullopt, 2, 2, '"', 'p', VTType::VT220, "DECSCL", "Set conformance level (DECSCL), VT220 and up.");
// constexpr inline auto DECSCPP     = detail::CSI(std::nullopt, 0, 1, '$', '|', VTType::VT100, "DECSCPP", "Select 80 or 132 Columns per Page");
// constexpr inline auto DECSNLS     = detail::CSI(std::nullopt, 0, 1, '*', '|', VTType::VT420, "DECSNLS", "Select number of lines per screen.");
// constexpr inline auto DECSCUSR    = detail::CSI(std::nullopt, 0, 1, ' ', 'q', VTType::VT520, "DECSCUSR", "Set Cursor Style");
// constexpr inline auto DECSLRM     = detail::CSI(std::nullopt, 0, 2, std::nullopt, 's', VTType::VT420, "DECSLRM", "Set left/right margin");
// constexpr inline auto DECSM       = detail::CSI('?', 1, ArgsMax, std::nullopt, 'h', VTType::VT100, "DECSM", "Set DEC-mode");
// constexpr inline auto DECSTBM     = detail::CSI(std::nullopt, 0, 2, std::nullopt, 'r', VTType::VT100, "DECSTBM", "Set top/bottom margin");
// constexpr inline auto DECSTR      = detail::CSI(std::nullopt, 0, 0, '!', 'p', VTType::VT100, "DECSTR", "Soft terminal reset");
// constexpr inline auto DECXCPR     = detail::CSI(std::nullopt, 0, 0, std::nullopt, '6', VTType::VT100, "DECXCPR", "Request extended cursor position");
// constexpr inline auto DL          = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'M', VTType::VT100, "DL",  "Delete lines");
// constexpr inline auto ECH         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'X', VTType::VT420, "ECH", "Erase characters");
// constexpr inline auto ED          = detail::CSI(std::nullopt, 0, ArgsMax, std::nullopt, 'J', VTType::VT100, "ED",  "Erase in display");
// constexpr inline auto EL          = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'K', VTType::VT100, "EL",  "Erase in line");
// constexpr inline auto HPA         = detail::CSI(std::nullopt, 1, 1, std::nullopt, '`', VTType::VT100, "HPA", "Horizontal position absolute");
// constexpr inline auto HPR         = detail::CSI(std::nullopt, 1, 1, std::nullopt, 'a', VTType::VT100, "HPR", "Horizontal position relative");
// constexpr inline auto HVP         = detail::CSI(std::nullopt, 0, 2, std::nullopt, 'f', VTType::VT100, "HVP", "Horizontal and vertical position");
// constexpr inline auto ICH         = detail::CSI(std::nullopt, 0, 1, std::nullopt, '@', VTType::VT420, "ICH", "Insert character");
// constexpr inline auto IL          = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'L', VTType::VT100, "IL",  "Insert lines");
// constexpr inline auto REP         = detail::CSI(std::nullopt, 1, 1, std::nullopt, 'b', VTType::VT100, "REP", "Repeat the preceding graphic character Ps times");
// constexpr inline auto RM          = detail::CSI(std::nullopt, 1, ArgsMax, std::nullopt, 'l', VTType::VT100, "RM",  "Reset mode");
// constexpr inline auto SCOSC       = detail::CSI(std::nullopt, 0, 0, std::nullopt, 's', VTType::VT100, "SCOSC", "Save Cursor (available only when DECLRMM is disabled)");
// constexpr inline auto SD          = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'T', VTType::VT100, "SD",  "Scroll down (pan up)");
// constexpr inline auto SETMARK     = detail::CSI('>', 0, 0, std::nullopt, 'M', VTExtension::Contour, "XTSETMARK", "Set Vertical Mark (experimental syntax)");
// constexpr inline auto SGR         = detail::CSI(std::nullopt, 0, ArgsMax, std::nullopt, 'm', VTType::VT100, "SGR", "Select graphics rendition");
// constexpr inline auto SM          = detail::CSI(std::nullopt, 1, ArgsMax, std::nullopt, 'h', VTType::VT100, "SM",  "Set mode");
// constexpr inline auto SU          = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'S', VTType::VT100, "SU",  "Scroll up (pan down)");
// constexpr inline auto TBC         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'g', VTType::VT100, "TBC", "Horizontal Tab Clear");
// constexpr inline auto VPA         = detail::CSI(std::nullopt, 0, 1, std::nullopt, 'd', VTType::VT100, "VPA", "Vertical Position Absolute");
// constexpr inline auto WINMANIP    = detail::CSI(std::nullopt, 1, 3, std::nullopt, 't', VTExtension::XTerm, "WINMANIP", "Window Manipulation");
// constexpr inline auto XTSMGRAPHICS= detail::CSI('?', 2, 4, std::nullopt, 'S', VTExtension::XTerm, "XTSMGRAPHICS", "Setting/getting Sixel/ReGIS graphics settings.");
// constexpr inline auto XTPOPCOLORS    = detail::CSI(std::nullopt, 0, ArgsMax, '#', 'Q', VTExtension::XTerm, "XTPOPCOLORS", "Pops the color palette from the palette's saved-stack.");
// constexpr inline auto XTPUSHCOLORS   = detail::CSI(std::nullopt, 0, ArgsMax, '#', 'P', VTExtension::XTerm, "XTPUSHCOLORS", "Pushes the color palette onto the palette's saved-stack.");
// constexpr inline auto XTREPORTCOLORS = detail::CSI(std::nullopt, 0, 0, '#', 'R', VTExtension::XTerm, "XTREPORTCOLORS", "Reports number of color palettes on the stack.");
// constexpr inline auto XTSHIFTESCAPE=detail::CSI('>', 0, 1, std::nullopt, 's', VTExtension::XTerm, "XTSHIFTESCAPE", "Set/reset shift-escape options.");
// constexpr inline auto XTVERSION   = detail::CSI('>', 0, 1, std::nullopt, 'q', VTExtension::XTerm, "XTVERSION", "Query terminal name and version");
// constexpr inline auto XTCAPTURE   = detail::CSI('>', 0, 2, std::nullopt, 't', VTExtension::Contour, "XTCAPTURE", "Report screen buffer capture.");
//
// constexpr inline auto DECSSDT     = detail::CSI(std::nullopt, 0, 1, '$', '~', VTType::VT320, "DECSSDT", "Select Status Display (Line) Type");
// constexpr inline auto DECSASD     = detail::CSI(std::nullopt, 0, 1, '$', '}', VTType::VT420, "DECSASD", "Select Active Status Display");
// constexpr inline auto DECPS       = detail::CSI(std::nullopt, 3, 18, ',', '~', VTType::VT520, "DECPS", "Controls the sound frequency or notes");


CSI_FUNCTION( ANSISYSSC   ,std::nullopt, 0, 0, std::nullopt, 'u', VTType::VT100, "Save Cursor (ANSI.SYS)");
CSI_FUNCTION( CBT         ,std::nullopt, 0, 1, std::nullopt, 'Z', VTType::VT100, "Cursor Backward Tabulation");
CSI_FUNCTION( CHA         ,std::nullopt, 0, 1, std::nullopt, 'G', VTType::VT100, "Move cursor to column");
CSI_FUNCTION( CHT         ,std::nullopt, 0, 1, std::nullopt, 'I', VTType::VT100, "Cursor Horizontal Forward Tabulation");
CSI_FUNCTION( CNL         ,std::nullopt, 0, 1, std::nullopt, 'E', VTType::VT100, "Move cursor to next line");
CSI_FUNCTION( CPL         ,std::nullopt, 0, 1, std::nullopt, 'F', VTType::VT100, "Move cursor to previous line");
CSI_FUNCTION( CPR         ,std::nullopt, 1, 1, std::nullopt, 'n', VTType::VT100, "Request Cursor position");
CSI_FUNCTION( CUB         ,std::nullopt, 0, 1, std::nullopt, 'D', VTType::VT100, "Move cursor backward");
CSI_FUNCTION( CUD         ,std::nullopt, 0, 1, std::nullopt, 'B', VTType::VT100, "Move cursor down");
CSI_FUNCTION( CUF         ,std::nullopt, 0, 1, std::nullopt, 'C', VTType::VT100, "Move cursor forward");
CSI_FUNCTION( CUP         ,std::nullopt, 0, 2, std::nullopt, 'H', VTType::VT100, "Move cursor to position");
CSI_FUNCTION( CUU         ,std::nullopt, 0, 1, std::nullopt, 'A', VTType::VT100, "Move cursor up");
CSI_FUNCTION( DA1         ,std::nullopt, 0, 1, std::nullopt, 'c', VTType::VT100, "Send primary device attributes");
CSI_FUNCTION( DA2         ,'>', 0, 1, std::nullopt, 'c', VTType::VT100, "Send secondary device attributes");
CSI_FUNCTION( DA3         ,'=', 0, 1, std::nullopt, 'c', VTType::VT100, "Send tertiary device attributes");
CSI_FUNCTION( DCH         ,std::nullopt, 0, 1, std::nullopt, 'P', VTType::VT100, "Delete characters");
CSI_FUNCTION( DECCARA     ,std::nullopt, 5, ArgsMax, '$', 'r', VTType::VT420, "Change Attributes in Rectangular Area");
CSI_FUNCTION( DECCRA      ,std::nullopt, 0, 8, '$', 'v', VTType::VT420, "Copy rectangular area");
CSI_FUNCTION( DECERA      ,std::nullopt, 0, 4, '$', 'z', VTType::VT420, "Erase rectangular area");
CSI_FUNCTION( DECFRA      ,std::nullopt, 0, 5, '$', 'x', VTType::VT420, "Fill rectangular area");
CSI_FUNCTION( DECDC       ,std::nullopt, 0, 1, '\'', '~', VTType::VT420, "Delete column");
CSI_FUNCTION( DECIC       ,std::nullopt, 0, 1, '\'', '}', VTType::VT420, "Insert column");
CSI_FUNCTION( DECSCA      ,std::nullopt, 0, 1, '"', 'q', VTType::VT240, "Select Character Protection Attribute");
CSI_FUNCTION( DECSED      ,'?', 0, 1, std::nullopt, 'J', VTType::VT240, "Selective Erase in Display");
CSI_FUNCTION( DECSERA     ,std::nullopt, 0, 4, '$', '{', VTType::VT240, "Selective Erase in Rectangular Area");
CSI_FUNCTION( DECSEL      ,'?', 0, 1, std::nullopt, 'K', VTType::VT240, "Selective Erase in Line");
CSI_FUNCTION( XTRESTORE   ,'?', 0, ArgsMax, std::nullopt, 'r', VTExtension::XTerm, "Restore DEC private modes.");
CSI_FUNCTION( XTSAVE      ,'?', 0, ArgsMax, std::nullopt, 's', VTExtension::XTerm, "Save DEC private modes.");
CSI_FUNCTION( DECRM       ,'?', 1, ArgsMax, std::nullopt, 'l', VTType::VT100, "Reset DEC-mode");
CSI_FUNCTION( DECRQM      ,'?', 1, 1, '$', 'p', VTType::VT100, "Request DEC-mode");
CSI_FUNCTION( DECRQM_ANSI ,std::nullopt, 1, 1, '$', 'p', VTType::VT100, "Request ANSI-mode");// NOLINT
CSI_FUNCTION( DECRQPSR    ,std::nullopt, 1, 1, '$', 'w', VTType::VT320, "Request presentation state report");
CSI_FUNCTION( DECSCL      ,std::nullopt, 2, 2, '"', 'p', VTType::VT220, "Set conformance level (DECSCL), VT220 and up.");
CSI_FUNCTION( DECSCPP     ,std::nullopt, 0, 1, '$', '|', VTType::VT100, "Select 80 or 132 Columns per Page");
CSI_FUNCTION( DECSNLS     ,std::nullopt, 0, 1, '*', '|', VTType::VT420, "Select number of lines per screen.");
CSI_FUNCTION( DECSCUSR    ,std::nullopt, 0, 1, ' ', 'q', VTType::VT520, "Set Cursor Style");
CSI_FUNCTION( DECSLRM     ,std::nullopt, 0, 2, std::nullopt, 's', VTType::VT420, "Set left/right margin");
CSI_FUNCTION( DECSM       ,'?', 1, ArgsMax, std::nullopt, 'h', VTType::VT100, "Set DEC-mode");
CSI_FUNCTION( DECSTBM     ,std::nullopt, 0, 2, std::nullopt, 'r', VTType::VT100, "Set top/bottom margin");
CSI_FUNCTION( DECSTR      ,std::nullopt, 0, 0, '!', 'p', VTType::VT100, "Soft terminal reset");
CSI_FUNCTION( DECXCPR     ,std::nullopt, 0, 0, std::nullopt, '6', VTType::VT100, "Request extended cursor position");
CSI_FUNCTION( DL          ,std::nullopt, 0, 1, std::nullopt, 'M', VTType::VT100,  "Delete lines");
CSI_FUNCTION( ECH         ,std::nullopt, 0, 1, std::nullopt, 'X', VTType::VT420, "Erase characters");
CSI_FUNCTION( ED          ,std::nullopt, 0, ArgsMax, std::nullopt, 'J', VTType::VT100,  "Erase in display");
CSI_FUNCTION( EL          ,std::nullopt, 0, 1, std::nullopt, 'K', VTType::VT100,  "Erase in line");
CSI_FUNCTION( HPA         ,std::nullopt, 1, 1, std::nullopt, '`', VTType::VT100, "Horizontal position absolute");
CSI_FUNCTION( HPR         ,std::nullopt, 1, 1, std::nullopt, 'a', VTType::VT100, "Horizontal position relative");
CSI_FUNCTION( HVP         ,std::nullopt, 0, 2, std::nullopt, 'f', VTType::VT100, "Horizontal and vertical position");
CSI_FUNCTION( ICH         ,std::nullopt, 0, 1, std::nullopt, '@', VTType::VT420, "Insert character");
CSI_FUNCTION( IL          ,std::nullopt, 0, 1, std::nullopt, 'L', VTType::VT100,  "Insert lines");
CSI_FUNCTION( REP         ,std::nullopt, 1, 1, std::nullopt, 'b', VTType::VT100, "Repeat the preceding graphic character Ps times");
CSI_FUNCTION( RM          ,std::nullopt, 1, ArgsMax, std::nullopt, 'l', VTType::VT100,  "Reset mode");
CSI_FUNCTION( SCOSC       ,std::nullopt, 0, 0, std::nullopt, 's', VTType::VT100, "Save Cursor (available only when DECLRMM is disabled)");
CSI_FUNCTION( SD          ,std::nullopt, 0, 1, std::nullopt, 'T', VTType::VT100,  "Scroll down (pan up)");
CSI_FUNCTION( SETMARK     ,'>', 0, 0, std::nullopt, 'M', VTExtension::Contour, "Set Vertical Mark (experimental syntax)");
CSI_FUNCTION( SGR         ,std::nullopt, 0, ArgsMax, std::nullopt, 'm', VTType::VT100, "Select graphics rendition");
CSI_FUNCTION( SM          ,std::nullopt, 1, ArgsMax, std::nullopt, 'h', VTType::VT100,  "Set mode");
CSI_FUNCTION( SU          ,std::nullopt, 0, 1, std::nullopt, 'S', VTType::VT100,  "Scroll up (pan down)");
CSI_FUNCTION( TBC         ,std::nullopt, 0, 1, std::nullopt, 'g', VTType::VT100, "Horizontal Tab Clear");
CSI_FUNCTION( VPA         ,std::nullopt, 0, 1, std::nullopt, 'd', VTType::VT100, "Vertical Position Absolute");
CSI_FUNCTION( WINMANIP    ,std::nullopt, 1, 3, std::nullopt, 't', VTExtension::XTerm, "Window Manipulation");
CSI_FUNCTION( XTSMGRAPHICS,'?', 2, 4, std::nullopt, 'S', VTExtension::XTerm, "Setting/getting Sixel/ReGIS graphics settings.");
CSI_FUNCTION( XTPOPCOLORS    ,std::nullopt, 0, ArgsMax, '#', 'Q', VTExtension::XTerm, "Pops the color palette from the palette's saved-stack.");
CSI_FUNCTION( XTPUSHCOLORS   ,std::nullopt, 0, ArgsMax, '#', 'P', VTExtension::XTerm, "Pushes the color palette onto the palette's saved-stack.");
CSI_FUNCTION( XTREPORTCOLORS ,std::nullopt, 0, 0, '#', 'R', VTExtension::XTerm, "Reports number of color palettes on the stack.");
CSI_FUNCTION( XTSHIFTESCAPE,'>', 0, 1, std::nullopt, 's', VTExtension::XTerm, "Set/reset shift-escape options.");
CSI_FUNCTION( XTVERSION   ,'>', 0, 1, std::nullopt, 'q', VTExtension::XTerm, "Query terminal name and version");
CSI_FUNCTION( XTCAPTURE   ,'>', 0, 2, std::nullopt, 't', VTExtension::Contour, "Report screen buffer capture.");

CSI_FUNCTION( DECSSDT     ,std::nullopt, 0, 1, '$', '~', VTType::VT320, "Select Status Display (Line) Type");
CSI_FUNCTION( DECSASD     ,std::nullopt, 0, 1, '$', '}', VTType::VT420, "Select Active Status Display");
CSI_FUNCTION( DECPS       ,std::nullopt, 3, 18, ',', '~', VTType::VT520, "Controls the sound frequency or notes");

#undef CSI_FUNCTION
// DCS functions
constexpr inline auto STP         = detail::DCS(std::nullopt, 0, 0, '$', 'p', VTExtension::Contour, "XTSETPROFILE", "Set Terminal Profile");
constexpr inline auto DECRQSS     = detail::DCS(std::nullopt, 0, 0, '$', 'q', VTType::VT420, "DECRQSS", "Request Status String");
constexpr inline auto DECSIXEL    = detail::DCS(std::nullopt, 0, 3, std::nullopt, 'q', VTType::VT330, "DECSIXEL", "Sixel Graphics Image");
constexpr inline auto XTGETTCAP   = detail::DCS(std::nullopt, 0, 0, '+', 'q', VTExtension::XTerm, "XTGETTCAP", "Request Termcap/Terminfo String");

// OSC
constexpr inline auto SETTITLE      = detail::OSC(0, VTExtension::XTerm, "SETINICON", "Change Window & Icon Title");
constexpr inline auto SETICON       = detail::OSC(1, VTExtension::XTerm, "SETWINICON", "Change Icon Title");
constexpr inline auto SETWINTITLE   = detail::OSC(2, VTExtension::XTerm, "SETWINTITLE", "Change Window Title");
constexpr inline auto SETXPROP      = detail::OSC(3, VTExtension::XTerm, "SETXPROP", "Set X11 property");
constexpr inline auto SETCOLPAL     = detail::OSC(4, VTExtension::XTerm, "SETCOLPAL", "Set/Query color palette");
// TODO: Ps = 4 ; c ; spec -> Change Color Number c to the color specified by spec.
// TODO: Ps = 5 ; c ; spec -> Change Special Color Number c to the color specified by spec.
// TODO: Ps = 6 ; c ; f -> Enable/disable Special Color Number c.
// TODO: Ps = 7 (set current working directory)
constexpr inline auto SETCWD        = detail::OSC(7, VTExtension::XTerm, "SETCWD", "Set current working directory");
constexpr inline auto HYPERLINK     = detail::OSC(8, VTExtension::Unknown, "HYPERLINK", "Hyperlinked Text");
constexpr inline auto COLORFG       = detail::OSC(10, VTExtension::XTerm, "COLORFG", "Change or request text foreground color.");
constexpr inline auto COLORBG       = detail::OSC(11, VTExtension::XTerm, "COLORBG", "Change or request text background color.");
constexpr inline auto COLORCURSOR   = detail::OSC(12, VTExtension::XTerm, "COLORCURSOR", "Change text cursor color to Pt.");
constexpr inline auto COLORMOUSEFG  = detail::OSC(13, VTExtension::XTerm, "COLORMOUSEFG", "Change mouse foreground color.");
constexpr inline auto COLORMOUSEBG  = detail::OSC(14, VTExtension::XTerm, "COLORMOUSEBG", "Change mouse background color.");
constexpr inline auto SETFONT       = detail::OSC(50, VTExtension::XTerm, "SETFONT", "Get or set font.");
constexpr inline auto SETFONTALL    = detail::OSC(60, VTExtension::Contour, "SETFONTALL", "Get or set all font faces, styles, size.");
// printf "\033]52;c;$(printf "%s" "blabla" | base64)\a"
constexpr inline auto CLIPBOARD     = detail::OSC(52, VTExtension::XTerm, "CLIPBOARD", "Clipboard management.");
constexpr inline auto RCOLPAL       = detail::OSC(104, VTExtension::XTerm, "RCOLPAL", "Reset color full palette or entry");
constexpr inline auto COLORSPECIAL  = detail::OSC(106, VTExtension::XTerm, "COLORSPECIAL", "Enable/disable Special Color Number c.");
constexpr inline auto RCOLORFG      = detail::OSC(110, VTExtension::XTerm, "RCOLORFG", "Reset VT100 text foreground color.");
constexpr inline auto RCOLORBG      = detail::OSC(111, VTExtension::XTerm, "RCOLORBG", "Reset VT100 text background color.");
constexpr inline auto RCOLORCURSOR  = detail::OSC(112, VTExtension::XTerm, "RCOLORCURSOR", "Reset text cursor color.");
constexpr inline auto RCOLORMOUSEFG = detail::OSC(113, VTExtension::XTerm,"RCOLORMOUSEFG", "Reset mouse foreground color.");
constexpr inline auto RCOLORMOUSEBG = detail::OSC(114, VTExtension::XTerm,"RCOLORMOUSEBG", "Reset mouse background color.");
constexpr inline auto RCOLORHIGHLIGHTFG = detail::OSC(119, VTExtension::XTerm,"RCOLORHIGHLIGHTFG", "Reset highlight foreground color.");
constexpr inline auto RCOLORHIGHLIGHTBG = detail::OSC(117, VTExtension::XTerm,"RCOLORHIGHLIGHTBG", "Reset highlight background color.");
constexpr inline auto NOTIFY        = detail::OSC(777, VTExtension::XTerm,"NOTIFY", "Send Notification.");
constexpr inline auto DUMPSTATE     = detail::OSC(888, VTExtension::Contour, "DUMPSTATE", "Dumps internal state to debug stream.");

constexpr inline auto CaptureBufferCode = 314;

// clang-format on

// HACK to get older compiler work (GCC 9.4)
constexpr static auto allFunctionsArray() noexcept
{
    auto funcs = std::array {
        // C0
        EOT,
        BEL,
        BS,
        TAB,
        LF,
        VT,
        FF,
        CR,
        LS0,
        LS1,

        // ESC
        DECALN,
        DECBI,
        DECFI,
        DECKPAM,
        DECKPNM,
        DECRS,
        DECSC,
        HTS,
        IND,
        NEL,
        RI,
        RIS,
        SCS_G0_SPECIAL,
        SCS_G0_USASCII,
        SCS_G1_SPECIAL,
        SCS_G1_USASCII,
        SS2,
        SS3,

        // CSI
        ANSISYSSC,
        XTCAPTURE,
        CBT,
        CHA,
        CHT,
        CNL,
        CPL,
        CPR,
        CUB,
        CUD,
        CUF,
        CUP,
        CUU,
        DA1,
        DA2,
        DA3,
        DCH,
        DECCARA,
        DECCRA,
        DECDC,
        DECERA,
        DECFRA,
        DECIC,
        DECSCA,
        DECSED,
        DECSERA,
        DECSEL,
        XTRESTORE,
        XTSAVE,
        DECPS,
        DECRM,
        DECRQM,
        DECRQM_ANSI,
        DECRQPSR,
        DECSASD,
        DECSCL,
        DECSCPP,
        DECSCUSR,
        DECSLRM,
        DECSM,
        DECSNLS,
        DECSSDT,
        DECSTBM,
        DECSTR,
        DECXCPR,
        DL,
        ECH,
        ED,
        EL,
        HPA,
        HPR,
        HVP,
        ICH,
        IL,
        REP,
        RM,
        SCOSC,
        SD,
        SETMARK,
        SGR,
        SM,
        SU,
        TBC,
        VPA,
        WINMANIP,
        XTPOPCOLORS,
        XTPUSHCOLORS,
        XTREPORTCOLORS,
        XTSHIFTESCAPE,
        XTSMGRAPHICS,
        XTVERSION,

        // DCS
        STP,
        DECRQSS,
        DECSIXEL,
        XTGETTCAP,

        // OSC
        SETICON,
        SETTITLE,
        SETWINTITLE,
        SETXPROP,
        SETCOLPAL,
        SETCWD,
        HYPERLINK,
        COLORFG,
        COLORBG,
        COLORCURSOR,
        COLORMOUSEFG,
        COLORMOUSEBG,
        SETFONT,
        SETFONTALL,
        CLIPBOARD,
        RCOLPAL,
        COLORSPECIAL,
        RCOLORFG,
        RCOLORBG,
        RCOLORCURSOR,
        RCOLORMOUSEFG,
        RCOLORMOUSEBG,
        RCOLORHIGHLIGHTFG,
        RCOLORHIGHLIGHTBG,
        NOTIFY,
        DUMPSTATE,
    };
    return funcs;
}

inline auto allFunctions() noexcept
{
    static auto const funcs = []() constexpr {
        auto funcs = allFunctionsArray();
        crispy::sort(funcs, [](FunctionDefinition const& a, FunctionDefinition const& b) constexpr {
            return compare(a, b);
        });
        return funcs;
    }();

#if 0
    for (auto [a, b] : crispy::indexed(funcs))
        std::cout << fmt::format("{:>2}: {}\n", a, b);
#endif

    return funcs;
}

// Class to store all supported VT sequence and support properly enabling/disabling them
// The storage stores all available definition at all time and is partitioned into
// two parts first part contains all active sequences and last part contains all
// disabled sequences
class SupportedSequences
{
  private:
    [[nodiscard]] constexpr auto begin() noexcept { return _supportedSequences.data(); }

    [[nodiscard]] constexpr auto end() noexcept { return begin() + _lastIndex; }

    [[nodiscard]] constexpr auto cbegin() const noexcept { return _supportedSequences.data(); }

    [[nodiscard]] constexpr auto cend() const noexcept { return cbegin() + _lastIndex; }

  public:
    [[nodiscard]] constexpr gsl::span<FunctionDefinition const> allSequences() const noexcept
    {
        return gsl::span<FunctionDefinition const>(cbegin(), _supportedSequences.size());
    }

    [[nodiscard]] constexpr gsl::span<FunctionDefinition const> activeSequences() const noexcept
    {
        return gsl::span<FunctionDefinition const>(cbegin(), _lastIndex);
    }

    CRISPY_CONSTEXPR void reset(VTType vt) noexcept
    {
        // Partition the array such that first half contains all sequences with VTType less than or
        // equal to given VTTYpe.
        auto* itr = std::partition(
            begin(),
            _supportedSequences.data() + _supportedSequences.size(),
            [vt](const FunctionDefinition& value) noexcept { return value.conformanceLevel <= vt; });

        _lastIndex = std::distance(begin(), itr);
        gsl::span<FunctionDefinition> availableDefinition(begin(), _lastIndex);
        crispy::sort(
            availableDefinition,
            [](FunctionDefinition const& a, FunctionDefinition const& b) constexpr { return compare(a, b); });
    }

    CRISPY_CONSTEXPR void disableSequence(FunctionDefinition seq) noexcept
    {
        auto* seqIter = std::find(begin(), end(), seq);
        if (seqIter != end())
        {
            // Move the disabled sequence to the end of array, keep the rest of active sequences sorted
            std::rotate(seqIter, seqIter + 1, _supportedSequences.data() + _supportedSequences.size());
            --_lastIndex;
        }
    }

    CRISPY_CONSTEXPR void enableSequence(FunctionDefinition seq) noexcept
    {
        auto* const endArray = _supportedSequences.data() + _supportedSequences.size();
        auto* seqIter = std::find(end(), endArray, seq);
        if (seqIter != endArray)
        {
            // Maybe could be done better since rest of the data is sorted
            std::iter_swap(end(), seqIter);
            ++_lastIndex;
            gsl::span<FunctionDefinition> arr(begin(), end());
            crispy::sort(arr, [](FunctionDefinition const& a, FunctionDefinition const& b) constexpr {
                return compare(a, b);
            });
        }
    }

  private:
    std::array<FunctionDefinition, allFunctionsArray().size()> _supportedSequences = allFunctions();
    size_t _lastIndex = allFunctions().size(); // No of total active sequences
};

/// Selects a FunctionDefinition based on a FunctionSelector.
///
/// @return the matching FunctionDefinition or nullptr if none matched.
FunctionDefinition const* select(FunctionSelector const& selector,
                                 gsl::span<FunctionDefinition const> availableDefinition) noexcept;

/// Selects a FunctionDefinition based on given input Escape sequence fields.
///
/// @p intermediate an optional intermediate character between (0x20 .. 0x2F)
/// @p finalCharacter between 0x40 .. 0x7F
///
/// @notice multi-character intermediates are intentionally not supported.
///
/// @return the matching FunctionDefinition or nullptr if none matched.
inline FunctionDefinition const* selectEscape(char intermediate,
                                              char finalCharacter,
                                              gsl::span<FunctionDefinition const> availableDefinition)
{
    return select({ FunctionCategory::ESC, 0, 0, intermediate, finalCharacter }, availableDefinition);
}

/// Selects a FunctionDefinition based on given input control sequence fields.
///
/// @p leader an optional value between 0x3C .. 0x3F
/// @p argc number of arguments supplied
/// @p intermediate an optional intermediate character between (0x20 .. 0x2F)
/// @p finalCharacter between 0x40 .. 0x7F
///
/// @notice multi-character intermediates are intentionally not supported.
///
/// @return the matching FunctionDefinition or nullptr if none matched.
inline FunctionDefinition const* selectControl(char leader,
                                               int argc,
                                               char intermediate,
                                               char finalCharacter,
                                               gsl::span<FunctionDefinition const> availableDefinition)
{
    return select({ FunctionCategory::CSI, leader, argc, intermediate, finalCharacter }, availableDefinition);
}

/// Selects a FunctionDefinition based on given input control sequence fields.
///
/// @p id leading numeric identifier (such as 8 for hyperlink)
///
/// @notice multi-character intermediates are intentionally not supported.
///
/// @return the matching FunctionDefinition or nullptr if none matched.
inline FunctionDefinition const* selectOSCommand(int id,
                                                 gsl::span<FunctionDefinition const> availableDefinition)
{
    return select({ FunctionCategory::OSC, 0, id, 0, 0 }, availableDefinition);
}

} // namespace terminal

template <>
struct std::hash<terminal::FunctionDefinition>
{
    /// This is actually perfect hashing.
    constexpr uint32_t operator()(terminal::FunctionDefinition const& fun) const noexcept { return fun.id(); }
};

// {{{ fmtlib support
template <>
struct fmt::formatter<terminal::FunctionCategory>: fmt::formatter<std::string_view>
{
    auto format(const terminal::FunctionCategory value, format_context& ctx) -> format_context::iterator
    {
        using terminal::FunctionCategory;
        string_view name;
        switch (value)
        {
            case FunctionCategory::C0:
                name = "C0";
                break;
                ;
            case FunctionCategory::ESC:
                name = "ESC";
                break;
                ;
            case FunctionCategory::CSI:
                name = "CSI";
                break;
                ;
            case FunctionCategory::OSC:
                name = "OSC";
                break;
                ;
            case FunctionCategory::DCS:
                name = "DCS";
                break;
                ;
        }
        return formatter<string_view>::format(name, ctx);
    }
};

template <>
struct fmt::formatter<terminal::FunctionDefinition>: fmt::formatter<std::string>
{
    auto format(const terminal::FunctionDefinition f, format_context& ctx) -> format_context::iterator
    {
        std::string value;
        switch (f.category)
        {
            case terminal::FunctionCategory::C0:
                value = fmt::format("{}", crispy::escape(static_cast<uint8_t>(f.finalSymbol)));
                break;
            case terminal::FunctionCategory::ESC:
                value = fmt::format("{} {} {}",
                                    f.category,
                                    f.intermediate ? f.intermediate : ' ',
                                    f.finalSymbol ? f.finalSymbol : ' ');
                break;
            case terminal::FunctionCategory::OSC:
                value = fmt::format("{} {}", f.category, f.maximumParameters);
                break;
            case terminal::FunctionCategory::DCS:
            case terminal::FunctionCategory::CSI:
                if (f.minimumParameters == f.maximumParameters)
                    value = fmt::format("{} {} {}    {} {}",
                                        f.category,
                                        f.leader ? f.leader : ' ',
                                        f.minimumParameters,
                                        f.intermediate ? f.intermediate : ' ',
                                        f.finalSymbol);
                else if (f.maximumParameters == terminal::ArgsMax)
                    value = fmt::format("{} {} {}..  {} {}",
                                        f.category,
                                        f.leader ? f.leader : ' ',
                                        f.minimumParameters,
                                        f.intermediate ? f.intermediate : ' ',
                                        f.finalSymbol);
                else
                    value = fmt::format("{} {} {}..{} {} {}",
                                        f.category,
                                        f.leader ? f.leader : ' ',
                                        f.minimumParameters,
                                        f.maximumParameters,
                                        f.intermediate ? f.intermediate : ' ',
                                        f.finalSymbol);
                break;
        }
        return formatter<std::string>::format(value, ctx);
    }
};

template <>
struct fmt::formatter<terminal::FunctionSelector>: fmt::formatter<std::string>
{
    auto format(const terminal::FunctionSelector f, format_context& ctx) -> format_context::iterator
    {
        std::string value;
        // clang-format off
        switch (f.category)
        {
            case terminal::FunctionCategory::OSC:
                value = fmt::format("{} {}", f.category, f.argc);
                break;
            default:
                value = fmt::format("{} {} {} {} {}",
                                    f.category,
                                    f.leader ? f.leader : ' ',
                                    f.argc,
                                    f.intermediate ? f.intermediate : ' ',
                                    f.finalSymbol ? f.finalSymbol : ' ');
                break;
        }
        // clang-format on
        return formatter<std::string>::format(value, ctx);
    }
};
// }}}
