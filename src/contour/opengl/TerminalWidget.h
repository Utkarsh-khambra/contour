/**
 * This file is part of the "contour" project
 *   Copyright (c) 2019-2021 Christian Parpart <christian@parpart.family>
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

#include <contour/Actions.h>
#include <contour/Config.h>
#include <contour/TerminalDisplay.h>
#include <contour/TerminalSession.h>
#include <contour/helper.h>

#include <terminal/Color.h>
#include <terminal/Metrics.h>
#include <terminal/primitives.h>

#include <terminal_renderer/Renderer.h>

#include <QtCore/QFileSystemWatcher>
#include <QtCore/QPoint>
#include <QtCore/QTimer>
#include <QtGui/QOpenGLExtraFunctions>
#include <QtGui/QVector4D>
#include <QtQuick/QQuickItem>

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    #include <QtOpenGLWidgets/QOpenGLWidget>
#else
    #include <QtWidgets/QOpenGLWidget>
#endif
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QSystemTrayIcon>

#include <atomic>
#include <fstream>
#include <memory>
#include <optional>
#include <vector>

namespace contour::opengl
{

class OpenGLRenderer;

// It currently just handles one terminal inside, but ideally later it can handle
// multiple terminals in tabbed views as well tiled.
class TerminalWidget: public QQuickItem, public TerminalDisplay
{
    Q_OBJECT
    Q_PROPERTY(QString profile READ profileName WRITE setProfileName NOTIFY profileNameChanged)
    QML_ELEMENT

  public:
    explicit TerminalWidget(QQuickItem* parent = nullptr);

    TerminalWidget(QQuickItem* parent,
                   std::chrono::seconds _earlyExitThreshold,
                   std::string _profileName,
                   std::string _programPath,
                   ContourGuiApp& _app,
                   std::function<void()> _adaptSize,
                   std::function<void(bool)> _enableBackgroundBlur);

    ~TerminalWidget() override;

    static QSurfaceFormat surfaceFormat();

    void initializeGL();
    void resizeGL(int _width, int _height);
    void paintGL();

    // {{{ Input handling
    void keyPressEvent(QKeyEvent* _keyEvent) override;
    void wheelEvent(QWheelEvent* _wheelEvent) override;
    void mousePressEvent(QMouseEvent* _mousePressEvent) override;
    void mouseReleaseEvent(QMouseEvent* _mouseReleaseEvent) override;
    void mouseMoveEvent(QMouseEvent* _mouseMoveEvent) override;
    void focusInEvent(QFocusEvent* _event) override;
    void focusOutEvent(QFocusEvent* _event) override;
    void inputMethodEvent(QInputMethodEvent* _event) override;
    QVariant inputMethodQuery(Qt::InputMethodQuery _query) const override;
    bool event(QEvent* _event) override;
    // }}}

    // {{{ TerminalDisplay API
    void closeDisplay() override;
    void post(std::function<void()> _fn) override;

    // Attributes
    double refreshRate() const override;
    crispy::Point screenDPI() const override;
    bool isFullScreen() const override;
    terminal::ImageSize pixelSize() const override;
    terminal::ImageSize cellSize() const override;

    // (user requested) actions
    bool requestPermission(config::Permission _allowedByConfig, std::string_view _topicText) override;
    terminal::FontDef getFontDef() override;
    void bell() override;
    void copyToClipboard(std::string_view /*_data*/) override;
    void inspect() override;
    void doDumpState();
    void notify(std::string_view /*_title*/, std::string_view /*_body*/) override;
    void resizeWindow(terminal::LineCount, terminal::ColumnCount) override;
    void resizeWindow(terminal::Width, terminal::Height) override;
    void setFonts(terminal::renderer::FontDescriptions _fontDescriptions) override;
    bool setFontSize(text::font_size _size) override;
    bool setScreenSize(terminal::PageSize _newScreenSize) override;
    void setMouseCursorShape(MouseCursorShape _shape) override;
    void setWindowTitle(std::string_view /*_title*/) override;
    void setWindowFullScreen() override;
    void setWindowMaximized() override;
    void setWindowNormal() override;
    void setBlurBehind(bool _enable) override;
    void setBackgroundImage(std::optional<terminal::BackgroundImage> const& backgroundImage) override;
    void toggleFullScreen() override;
    void setHyperlinkDecoration(terminal::renderer::Decorator _normal,
                                terminal::renderer::Decorator _hover) override;
    void setBackgroundOpacity(terminal::Opacity _opacity) override;

    // terminal events
    void scheduleRedraw() override;
    void renderBufferUpdated() override;
    void onSelectionCompleted() override;
    void bufferChanged(terminal::ScreenType) override;
    void discardImage(terminal::Image const&) override;
    // }}}

    void releaseResources() override;

    QString profileName() const { return QString::fromStdString(profileName_); }
    void setProfileName(QString const& name) { profileName_ = name.toStdString(); }

  public Q_SLOTS:
    void handleWindowChanged(QQuickWindow* win);
    void cleanup();

    void onFrameSwapped();
    void onScrollBarValueChanged(int _value);
    void onRefreshRateChanged();
    void onScreenDpiChanged();
    void onScreenChanged();
    void onDpiConfigChanged();

  signals:
    void profileNameChanged();
    void terminalBufferChanged(terminal::ScreenType);
    void terminalBufferUpdated();
    void terminated();
    void showNotification(QString const& _title, QString const& _body);

  private:
    // helper methods
    //
    void sync();
    config::TerminalProfile const& profile() const noexcept { return session_->profile(); }
    terminal::Terminal& terminal() noexcept { return session_->terminal(); }
    void configureScreenHooks();
    void logDisplayInfo();
    void watchKdeDpiSetting();
    terminal::PageSize screenSize() const
    {
        return screenSizeForPixels(pixelSize(), renderer_.gridMetrics());
    }
    void assertInitialized();
    double contentScale() const;
    void updateMinimumSize();

    void statsSummary();
    void doResize(crispy::Size _size);
    terminal::renderer::GridMetrics const& gridMetrics() const noexcept { return renderer_.gridMetrics(); }

    /// Flags the screen as dirty.
    ///
    /// @returns boolean indicating whether the screen was clean before and made dirty (true), false
    /// otherwise.
    bool setScreenDirty()
    {
#if defined(CONTOUR_PERF_STATS)
        stats_.updatesSinceRendering++;
#endif
        return state_.touch();
    }

    // data members
    //
    ContourGuiApp& app_;
    config::Config config_;
    const bool liveConfig_;
    std::string profileName_;
    std::string programPath_;
    std::unique_ptr<TerminalSession> session_;

    // data fields
    //
    std::function<void()> adaptSize_;
    std::function<void(bool)> enableBlurBehind_;
    terminal::renderer::Renderer renderer_;
    std::atomic<bool> initialized_ = false;
    bool renderingPressure_ = false;
    OpenGLRenderer* renderTarget_ = nullptr;
    PermissionCache rememberedPermissions_ {};
    bool maximizedState_ = false;

    // update() timer used to animate the blinking cursor.
    QTimer updateTimer_;

    RenderStateManager state_;

    QFileSystemWatcher filesystemWatcher_;
    crispy::Point lastScreenDPI_;

    // ======================================================================

#if defined(CONTOUR_PERF_STATS)
    struct Stats
    {
        std::atomic<uint64_t> updatesSinceRendering = 0;
        std::atomic<uint64_t> consecutiveRenderCount = 0;
    };
    Stats stats_;
    std::atomic<uint64_t> renderCount_ = 0;
#endif
};

} // namespace contour::opengl
