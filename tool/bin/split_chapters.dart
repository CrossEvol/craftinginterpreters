import 'package:tool/src/book.dart';
import 'package:tool/src/split_chapter.dart';

void main(List<String> arguments) {
  var book = Book();
  for (var page in book.pages) {
    splitChapter(book, page);
  }
}
