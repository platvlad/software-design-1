# Review архитектуры
Сложно оценивать архитектурные решения на функциональном языке 
программирования, но хотелось бы отметить высокий уровень 
модульности, удобное распределение кода по компонентам Command 
и Environment, например вынесение функций обработки потока в 
StreamOperations.hs, что позволяет писать обработчики команд
макимум в несколько строк. Организация сборки и 
тестирования позволяет одной строчкой запустить все тесты. 
Немного неочевидной является работа парсера в том месте, где
проверяется соответствие префикса "cat ", "pwd " и так далее.
Непонятно, почему парсер должен распознать строку "pwd", 
написанную без пробелов. Но кажется всё работает, поэтому 
тут можно отметить разве что небольшую неявность происходящего.

В целом архитектура позволяет добавлять новые команды, 
потратив на каждую несколько строчек кода и несколько строчек 
на тесты. Хотя сложность языка реализации может сделать и это
непростой задачей.
