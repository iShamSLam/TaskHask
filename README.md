# ФП/Haskell

Ещё один способ получить баллы по ФП - решить задачи на Haskell.

В директории `src` лежат файлы `PartN.hs`. Задачи находятся либо в них, либо в
файлах соответствующих поддиректорий `PartN`. Как правило, требуется написать
какую-либо функцию или набор функций. Пожалуйста, не меняйте тип функций, иначе
может сломаться компиляция или проверка тестов. Тесты, кстати, лежат в
директории `test`, и могут содержать информацию о том, как должна работать
функция.

Для проверки можно запустить `cabal v2-test` или `stack test`.

На сервере для проверки будет запускаться дополнительный набор тестов, который
не сохранён в репозитории (файлы вида `test/HiddenN.hs` - как раз для них).
Также на сервере будут заменяться на исходные все файлы, в начале которых
написано `DO NOT CHANGE!`, поэтому менять их не имеет смысла. Вы можете добавить
свои тесты в файлы `TestN.hs`, чтобы проверить свой код.

Успехов!

It's Dangerous to Solve Problems Alone. Take [Type Checker.](https://haskell.org)
