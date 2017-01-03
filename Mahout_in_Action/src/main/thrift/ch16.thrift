namespace java mia.classifier.ch16.generated

service Classifier {
  // classify a text
  list<double> classify(1: string text),
}
