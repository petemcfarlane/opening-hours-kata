import Test.Hspec
import Data.Time
import OpeningHours

main :: IO ()
main = hspec $ do
    describe "opening hours" $ do
        it "should be open Wednesday" $
            isOpen shop wednesday `shouldBe` True
        it "should be closed at 7am Wednesday" $
            isOpen shop earlyWednesday `shouldBe` False
        it "should be closed Thursday" $
            isOpen shop thursday `shouldBe` False
        it "should be open Friday at 8am" $
            isOpen shop fridayMorning `shouldBe` True
        it "should be closed Friday at 6pm" $
            isOpen shop fridayEvening `shouldBe` False
    describe "next opening date" $ do
        it "should be next open Monday at 8 after Friday evening" $
            nextOpen shop fridayEvening `shouldBe` UTCTime (fromGregorian 2016 5 16) (secondsInHours 8)
        it "should be next open Wednesday at 8 after earlyWednesday" $
            nextOpen shop earlyWednesday `shouldBe` UTCTime (fromGregorian 2016 5 11) (secondsInHours 8)

shop :: Shop
shop = Shop [Mon,Wed,Fri] (secondsInHours 8, secondsInHours 16)

wednesday = UTCTime (fromGregorian 2016 5 11) 44532
earlyWednesday = UTCTime (fromGregorian 2016 5 11) (secondsInHours 7)
thursday = UTCTime (fromGregorian 2016 5 12) 44532
fridayMorning = UTCTime (fromGregorian 2016 5 13) (secondsInHours 8)
fridayEvening = UTCTime (fromGregorian 2016 5 13) (secondsInHours 18)

secondsInHours :: Int -> DiffTime
secondsInHours = realToFrac . (*) 3600
