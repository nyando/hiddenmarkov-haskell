import Test.Hspec
import HiddenMarkovModel

initial = [1.0, 0.0, 0.0]

transitions = [[0.0, 1.0, 0.0],
               [0.0, 0.0, 1.0],
               [1.0, 0.0, 0.0]]

emissions0 = [(0.5, 'R'), (0.5, 'G')]
emissions1 = [(0.5, 'G'), (0.5, 'B')]
emissions2 = [(0.5, 'B'), (0.5, 'R')]

detEmissions0 = [(1.0, 'R')]
detEmissions1 = [(1.0, 'G')]
detEmissions2 = [(1.0, 'B')]

s0 = HS 0 emissions0
s1 = HS 1 emissions1
s2 = HS 2 emissions2

detS0 = HS 0 detEmissions0
detS1 = HS 1 detEmissions1
detS2 = HS 2 detEmissions2

testHMM = HMM s0 [s0, s1, s2] transitions

detHMM = HMM detS0 [detS0, detS1, detS2] transitions
detInput = [(0.8, 0.3), (0.2, 0.1), (0.4, 0.5)]

main :: IO ()
main = hspec $ do
  describe "HiddenMarkovModel.generate" $ do
    it "generates string RGB" $ do
      generate detInput detHMM `shouldBe` "RGB"

  describe "HiddenMarkovModel.viterbi" $ do
    it "calculates the correct state sequence for string RGB" $ do
      viterbi "RGB" testHMM initial `shouldBe` [0, 1, 2]

  describe "HiddenMarkovModel.forward" $ do
    it "calculates the correct probability for string RGB" $ do
      forward "RGB" testHMM initial `shouldBe` 0.125
