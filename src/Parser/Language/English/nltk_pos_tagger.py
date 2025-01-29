from nltk.tag import pos_tag
from nltk.tokenize import word_tokenize
import json

def pos_tagger(text):
  tokens = word_tokenize(text)
  tagged_tokens = pos_tag(tokens)
  json_data = [{'word':word, 'pos':tag} for word, tag in tagged_tokens]
  json_str = json.dumps(json_data)
  return json_str

# if __name__ == "__main__":
text = input().strip()
print(pos_tagger(text))


