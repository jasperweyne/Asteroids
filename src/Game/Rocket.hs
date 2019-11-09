module Game.Rocket (updateRockets) where
    import Type.Object.Rocket 
    import Type.State
    
    updateRockets :: Float -> GameState -> [Rocket]
    updateRockets t gs = rs
        where 
            rs = (rockets.inGame) gs