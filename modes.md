A mode consists of the following information
* a name for the mode
  * I think we could also have a notion of submodes, either as making a submode
    itself a fully-featured mode, or as some kind of declaration within the
    parent mode
* global state which the mode will use
  * thinking some kind of notion of global state which allows for communication
    between modes
* local state that should persist after a mode switch
* an initialization routine for when the mode is switched to
  * things like graphics are implicitly loaded when mode is switched to
  * designed for setting up the screen (e.g. the programmer writes text on the
    screen to be displayed, for example)
* a destruction routine for if the mode switches away
  * not exactly what this would be useful for, but i think it could be a good
    utility
* **a main loop which will be run every frame**:
  * this is what actually contains the code that performs computations
  * can access controller inputs and do things to display elements based on
    that
* graphics that will be used during a mode
  * does this mean we necessarily load these graphics to VRAM? what if not all
    graphics are used? what if the user wants too many graphics loaded?
* sprites types (?) which will be in use during a mode
  * I am thinking something like objects (in the OO sense) for sprites. they
    have their own state (like a record) and also own the graphics slots it
    needs to display itself
  * should the sprites be aware of the graphics it needs? does that need to be
    consistent with the graphics declarations from above?
* foreground/background? or does this make more sense to have the user command
  these things in the main loop
* sound system? (I know nothing about sound)

As an example, I'm thinking of something like:

```
{-
This describes a sample main menu, which I would imagine looks something like
  |----------------|
  |                |
  |   GAME LOGO!   |
  |                |
  |  >Press start  |
  |                |
  | High score: 50 |
  |                |
  |----------------|
and the text that says "press start" flashes. And if you press start, the game
switches modes to something else
-}
mode MainMenu {
  global high_score -- this might be defined somewhere else, and this acts
                    -- something like `extern` in C

  graphics {
    font, logo
  }

  state visible = false

  init {
    -- the init process would also implicitly load graphics and initialize
    -- state as above

    <code to put game logo on screen>
    <code to put high score text on screen>
  }

  loop {  -- the single loop that would run every frame
    if con1.pressed(BUTTON_START) {
      switch IntroCutscene  -- changes the mode
    }

    every 40 { -- every 40 frames (does it need to be inside loop decl?)
      if visible then {
        <code to clear "press start" text>
        visible <- false
      } else {
        <code to display "press start" text>
        visible <- true
      }
    }
  }
}
```
