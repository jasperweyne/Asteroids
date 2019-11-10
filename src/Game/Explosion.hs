module Game.Explosion (updateExplosions, postUpdateExplosions) where
    
  import Class.HasGameObject
  import Data.Maybe
  import Type.Object.Explosion
  import Type.Physics.GameObject
  import Type.State
  import Type.Object.Player as Player
  import Type.IO.Input
  import Game.Object
  import Physics.Collisions
  import Type.Rendering.Animation
  
  --Update explosion objects
  updateExplosions :: Float -> GameState -> [Explosion]
  updateExplosions t gs@GameState{inputState = ks, inGame = igs}
    = mapMaybe (updateExplosion t) (explosions igs) ++ createExplosions gs

  --Update explosion animation
  updateExplosion :: Float -> Explosion -> Maybe Explosion
  updateExplosion t ex@Explosion{anim = an}
    | currenttime an >= 1 = Nothing
    | otherwise           = Just ex

  --Create new explosions for given collisions
  createExplosions :: GameState -> [Explosion]
  createExplosions gs@GameState{inGame=igs@InGameState{asteroids=as, pRockets=pr, sRockets=sr, player=p}} =
    playerExpl ++ mapMaybe saucerExpl (saucers igs) ++ mapMaybe rocketExpl pr ++ mapMaybe rocketExpl sr
      where
        mkExpl x = makeExplosion (pos . getGameObject $ x) (explosion gs)
        saucerExpl x | x `collidesWith` as || x `collidesWith` pr = Just (mkExpl x)
                     | otherwise = Nothing
        playerExpl   | p `collidesWith` as || p `collidesWith` sr = [mkExpl p]
                     | otherwise = []
        rocketExpl x | x `collidesWith` as = Just (mkExpl x)
                     | otherwise = Nothing

  postUpdateExplosions :: Float -> GameState -> GameState
  postUpdateExplosions t gs = gs