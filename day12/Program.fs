open System
open System.IO

let readFile (filePath: String) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


type Adj(com: char, d:int64) as self =
  member this.d = d
  member this.com = com
  new (s:String) =
        let c = s.[0]
        let num = s.[1..] |> int64
        Adj(c,num)

type Dir(d: char) as self =
    let rotRight () =
        match d with
        | 'N' -> Dir('E')
        | 'E' -> Dir('S')
        | 'S' -> Dir('W')
        | 'W' -> Dir('N')
    let rotLeft () =
        match d with
        | 'N' -> Dir('W')
        | 'W' -> Dir('S')
        | 'S' -> Dir('E')
        | 'E' -> Dir('N')
    member this.rotR(steps:int64) : Dir =
        if steps = 0L then
            self 
        else
            (rotRight ()).rotR(steps - 1L)
    member this.rotL(steps:int64) : Dir =
        if steps = 0L then
            self
        else
            (rotLeft ()).rotL(steps - 1L)
    member this.dir = d 
 
type Pos(x: int64, y: int64, dir: Dir) as self =
    member this.x = x
    member this.y = y
    member this.quadrant () : int =
        if x >= 0L && y < 0L then 1
        else if x >= 0L && y >= 0L then 2
        else if x <= 0L && y >= 0L then 3
        else 4 
    member this.dir = dir
    new() = Pos(0L,0L,Dir('E'))
    member this.adjust(adj:Adj):Pos =
        match adj.com with
        | 'N' -> Pos(x,y-adj.d,dir)
        | 'S' -> Pos(x,y+adj.d,dir)
        | 'E' -> Pos(x+adj.d,y,dir)
        | 'W' -> Pos(x-adj.d,y,dir)
        | 'R' -> Pos(x,y,dir.rotR(adj.d/90L))
        | 'L' -> Pos(x,y,dir.rotL(adj.d/90L))
        | 'F' -> this.adjust(Adj(dir.dir,adj.d))
        
    member this.adjustRelative(adj:Adj):Pos =
        match adj.com with
        | 'N' -> Pos(x,y-adj.d,dir)
        | 'S' -> Pos(x,y+adj.d,dir)
        | 'E' -> Pos(x+adj.d,y,dir)
        | 'W' -> Pos(x-adj.d,y,dir)
        | 'R' -> self.rotateRelativeR (adj.d/90L)
        | 'L' -> self.rotateRelativeL (adj.d/90L)       
    member this.rotateRelativeR (steps:int64) : Pos =
        if steps = 0L then self 
        else Pos(-y,x,dir).rotateRelativeR (steps-1L)
    member this.rotateRelativeL (steps:int64) : Pos =
        if steps = 0L then self 
        else Pos(y,-x,dir).rotateRelativeL (steps-1L)
            
        
    override this.ToString() =
        sprintf "(%d,%d,%c)" x y dir.dir
    member this.manhattan () =
        abs(x) + abs(y) 

type Ship(pos:Pos, waypoint: Pos) as self =
    member this.pos = pos
    member this.waypoint = pos
    new () = Ship(Pos(0L,0L,Dir('E')), Pos(10L,-1L,Dir('E')))
    member this.adjust (adj: Adj) =
        match adj.com with
        | 'N' -> Ship(pos, waypoint.adjustRelative adj)
        | 'S' -> Ship(pos, waypoint.adjustRelative adj)
        | 'E' -> Ship(pos, waypoint.adjustRelative adj)
        | 'W' -> Ship(pos, waypoint.adjustRelative adj)
        | 'R' -> Ship(pos, waypoint.adjustRelative adj)
        | 'L' -> Ship(pos, waypoint.adjustRelative adj)
        | 'F' -> this.forward adj.d
    member this.forward (steps:int64) : Ship =
        let dx = waypoint.x * steps 
        let dy = waypoint.y * steps 
        let newPos = Pos(pos.x+dx,pos.y + dy,pos.dir)      
        Ship(newPos,waypoint)
    override this.ToString () =
        sprintf "Ship(%A wp=%A)" pos waypoint 
        
[<EntryPoint>]
let main argv =
    let input:Adj[] =
        readFile "/Users/xeno/projects/aoc2020/day12_fs/input.txt"
        |> Seq.map Adj |> Seq.toArray
    let start = Pos()
    let iter1 = start.adjust input.[0]
    printfn "%A" iter1
    let endPos = input |> Seq.fold (fun (pos:Pos) -> pos.adjust) start 
    printfn "%A manhattan=%d" endPos (endPos.manhattan ()) 

    let ship : Ship = Ship ()
    let endShip = input |> Seq.fold (fun (s:Ship) -> printfn "%A" s ; s.adjust) ship
    printfn "%A manhattan=%d" endShip (endShip.pos.manhattan ()) 
    

    0 // return an integer exit code
    
    