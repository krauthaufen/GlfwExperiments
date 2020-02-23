open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Base.Rendering
open Glfw

[<EntryPoint;STAThread>]
let main argv = 

    let win = 
        Window.create {
            title = "Dummy"            
            width = 1024
            height = 768
            resizable = true
            focus = true
            refreshRate = 0
            opengl = Some(4,1)
        }

    win.KeyDown.Add(fun e ->
        if e.Key = Silk.NET.GLFW.Keys.Escape then win.Close()
        else Log.warn "down %A (%d)" e.Key (int e.Key)
    )
    win.KeyUp.Add(fun e ->
        Log.warn "up %A" e.Key
    )

    win.Run()
    // Ag.initialize()
    // Aardvark.Init()

    // Window.create()


    0
