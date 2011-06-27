#|
The Racket version of Jeff Molofee (NeHe) "OpenGL Tutorials".
Lessons 01-05.

Copyright (c) 2011  Mikhail Mosienko  <netluxe@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

#lang racket/gui

(require sgl/gl)

(define lesson-1-canvas%
  (class canvas%
    (inherit refresh with-gl-context swap-gl-buffers get-width get-height)
    (super-new [style '(gl no-autoclear)])

    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         ; очистка Экрана и буфера глубины
         (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
         ; Сброс просмотра
         (glLoadIdentity)
         (swap-gl-buffers))))

    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         ; Предотвращение деления на ноль, если окно слишком мало
         (when (zero? height) (set! height 1))
         ; Сброс текущей области вывода и перспективных преобразований
         (glViewport 0 0 width height)
         ; Выбор матрицы проекций
         (glMatrixMode GL_PROJECTION)
         ; Сброс матрицы проекции
         (glLoadIdentity)
         ; Вычисление соотношения геометрических размеров для окна
         (gluPerspective 45.0 (/ width height) 0.1 100.0)
         ; Выбор матрицы просмотра модели
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)))
      (refresh))

    (define (gl-init width height)
      (with-gl-context
       (lambda ()
         ; Очистка экрана в черный цвет
         (glClearColor 0.0 0.0 0.0 0.0)
         ; Разрешить очистку буфера глубины
         (glClearDepth 1.0)
         ; Тип теста глубины
         (glDepthFunc GL_LESS)
         ; разрешить тест глубины
         (glEnable GL_DEPTH_TEST)
         ; разрешить плавное цветовое сглаживание
         (glShadeModel GL_SMOOTH)
         ; Выбор матрицы проекций
         (glMatrixMode GL_PROJECTION)
         ; Сброс матрицы проекции
         (glLoadIdentity)
         ; Вычисление соотношения геометрических размеров для окна
         (gluPerspective 45.0 (/ width height) 0.1 100.0)
         ; Выбор матрицы просмотра модели
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity))))

    (gl-init (get-width) (get-height))))

(define lesson-2-canvas%
  (class lesson-1-canvas%
    (inherit with-gl-context swap-gl-buffers)
    (super-new)

    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         ; очистка Экрана и буфера глубины
         (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
         ; Сброс просмотра
         (glLoadIdentity)
         ; Сдвинемся влево на 1.5 единицы и в экран на 6.0
         (glTranslatef -1.5 0.0 -6.0)
         (glBegin GL_TRIANGLES)
         (begin
           (glVertex3f  0.0  1.0 0.0)  ; Верхняя точка
           (glVertex3f -1.0 -1.0 0.0)  ; Левая точка
           (glVertex3f  1.0 -1.0 0.0)) ; Правая точка
         (glEnd)
         ; Сдвинем вправо на 3 единицы
         (glTranslatef 3.0 0.0 0.0)
         (glBegin GL_QUADS)
         (begin
           (glVertex3f -1.0  1.0 0.0)  ; Верхняя левая
           (glVertex3f  1.0  1.0 0.0)  ; Верхняя правая
           (glVertex3f  1.0 -1.0 0.0)  ; Нижняя левая
           (glVertex3f -1.0 -1.0 0.0)) ; Нижняя правая
         (glEnd)
         (swap-gl-buffers))))))

(define lesson-3-canvas%
  (class lesson-1-canvas%
    (inherit with-gl-context swap-gl-buffers)
    (super-new)

    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         ; очистка Экрана и буфера глубины
         (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
         ; Сброс просмотра
         (glLoadIdentity)
         ; Сдвинемся влево на 1.5 единицы и в экран на 6.0
         (glTranslatef -1.5 0.0 -6.0)
         (glBegin GL_TRIANGLES)
         (begin
           (glColor3f   1.0  0.0 0.0)  ; Красный цвет
           (glVertex3f  0.0  1.0 0.0)  ; Верхняя точка
           (glColor3f   0.0  1.0 0.0)  ; Зеленный цвет
           (glVertex3f -1.0 -1.0 0.0)  ; Левая точка
           (glColor3f   0.0  0.0 1.0)  ; Синий цвет
           (glVertex3f  1.0 -1.0 0.0)) ; Правая точка
         (glEnd)
         ; Сдвинем вправо на 3 единицы
         (glTranslatef 3.0 0.0 0.0)
         ; Установим синий цвет только один раз
         (glColor3f 0.5 0.5 1.0)
         (glBegin GL_QUADS)
         (begin
           (glVertex3f -1.0  1.0 0.0)  ; Верхняя левая
           (glVertex3f  1.0  1.0 0.0)  ; Верхняя правая
           (glVertex3f  1.0 -1.0 0.0)  ; Нижняя левая
           (glVertex3f -1.0 -1.0 0.0)) ; Нижняя правая
         (glEnd)
         (swap-gl-buffers))))))

(define lesson-4-canvas%
  (class lesson-1-canvas%
    (inherit refresh with-gl-context swap-gl-buffers)

    (field [rtri 0.0]   ; Угол вращения треугольника
           [rquad 0.0]) ; Угол вращения четырехугольника

    (init-field [rtri-offset 2.0]   ; Смещение угола вращения треугольника
                [rquad-offset -4.0]) ; Смещение угола вращения четырехугольника

    (super-new)

    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         ; очистка Экрана и буфера глубины
         (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
         ; Сброс просмотра
         (glLoadIdentity)
         ; Сдвинемся влево на 1.5 единицы и в экран на 6.0
         (glTranslatef -1.5 0.0 -6.0)
         ; Вращение треугольника по оси Y
         (glRotatef rtri 0.0 1.0 0.0)
         (glBegin GL_TRIANGLES)
         (begin
           (glColor3f   1.0  0.0 0.0)  ; Красный цвет
           (glVertex3f  0.0  1.0 0.0)  ; Верхняя точка
           (glColor3f   0.0  1.0 0.0)  ; Зеленный цвет
           (glVertex3f -1.0 -1.0 0.0)  ; Левая точка
           (glColor3f   0.0  0.0 1.0)  ; Синий цвет
           (glVertex3f  1.0 -1.0 0.0)) ; Правая точка
         (glEnd)
         (glLoadIdentity)
         (glTranslatef 1.5 0.0 -6.0)   ; Сдвиг вправо на 1.5
         (glRotatef rquad 1.0 0.0 0.0) ; Вращение по оси X
         ; Установим синий цвет только один раз
         (glColor3f 0.5 0.5 1.0)
         (glBegin GL_QUADS)
         (begin
           (glVertex3f -1.0  1.0 0.0)  ; Верхняя левая
           (glVertex3f  1.0  1.0 0.0)  ; Верхняя правая
           (glVertex3f  1.0 -1.0 0.0)  ; Нижняя левая
           (glVertex3f -1.0 -1.0 0.0)) ; Нижняя правая
         (glEnd)
         (swap-gl-buffers))))

    (letrec ([f (lambda()
                  (sleep 0.1)
                  (set! rtri (+ rtri rtri-offset))    ; Изменение угла вращения для треугольника
                  (set! rquad (+ rquad rquad-offset)) ; Изменение угла вращения для квадрата
                  (refresh)
                  (f))])
      (thread f))))

(define lesson-5-canvas%
  (class lesson-4-canvas%
    (inherit refresh with-gl-context swap-gl-buffers)
    (inherit-field rtri rquad)

    (super-new)

    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         ; очистка Экрана и буфера глубины
         (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
         ; Сброс просмотра
         (glLoadIdentity)
         ; Сдвинемся влево на 1.5 единицы и в экран на 6.0
         (glTranslatef -1.5 0.0 -6.0)
         ; Вращение пирамиды по оси Y
         (glRotatef rtri 0.0 1.0 0.0)
         (glBegin GL_TRIANGLES)
         (begin
           (glColor3f   1.0  0.0 0.0) ; Красный
           (glVertex3f  0.0  1.0 0.0) ; Верх треугольника (Передняя грань)
           (glColor3f   0.0  1.0 0.0) ; Зеленный
           (glVertex3f -1.0 -1.0 1.0) ; Левая точка
           (glColor3f   0.0  0.0 1.0) ; Синий
           (glVertex3f  1.0 -1.0 1.0) ; Правая точка

           (glColor3f  1.0  0.0  0.0) ; Красная
           (glVertex3f 0.0  1.0  0.0) ; Верх треугольника (Правая грань)
           (glColor3f  0.0  0.0  1.0) ; Синия
           (glVertex3f 1.0 -1.0  1.0) ; Лево треугольника (Правая грань)
           (glColor3f  0.0  1.0  0.0) ; Зеленная
           (glVertex3f 1.0 -1.0 -1.0) ; Право треугольника (Правая грань)

           (glColor3f   1.0  0.0  0.0) ; Красный
           (glVertex3f  0.0  1.0  0.0) ; Низ треугольника (Сзади)
           (glColor3f   0.0  1.0  0.0) ; Зеленный
           (glVertex3f  1.0 -1.0 -1.0) ; Лево треугольника (Сзади)
           (glColor3f   0.0  0.0  1.0) ; Синий
           (glVertex3f -1.0 -1.0 -1.0) ; Право треугольника (Сзади)

           (glColor3f   1.0  0.0  0.0) ; Красный
           (glVertex3f  0.0  1.0  0.0) ; Верх треугольника (Лево)
           (glColor3f   0.0  0.0  1.0) ; Синий
           (glVertex3f -1.0 -1.0 -1.0) ; Лево треугольника (Лево)
           (glColor3f   0.0  1.0  0.0) ; Зеленный
           (glVertex3f -1.0 -1.0  1.0) ; Право треугольника (Лево)
           )
         (glEnd)
         (glLoadIdentity)
         (glTranslatef 1.5 0.0 -7.0)   ; Сдвинуть вправо и вглубь экрана
         (glRotatef rquad 1.0 1.0 1.0) ; Вращение куба по X, Y и Z
         (glBegin GL_QUADS)
         (begin
           (glColor3f   0.0 1.0  0.0) ; Синий
           (glVertex3f  1.0 1.0 -1.0) ; Право верх квадрата (Верх)
           (glVertex3f -1.0 1.0 -1.0) ; Лево верх
           (glVertex3f -1.0 1.0  1.0) ; Лево низ
           (glVertex3f  1.0 1.0  1.0) ; Право низ

           (glColor3f   1.0  0.5  0.0) ; Оранжевый
           (glVertex3f  1.0 -1.0  1.0) ; Верх право квадрата (Низ)
           (glVertex3f -1.0 -1.0  1.0) ; Верх лево
           (glVertex3f -1.0 -1.0 -1.0) ; Низ лево
           (glVertex3f  1.0 -1.0 -1.0) ; Низ право

           (glColor3f   1.0  0.0 0.0) ; Красный
           (glVertex3f  1.0  1.0 1.0) ; Верх право квадрата (Перед)
           (glVertex3f -1.0  1.0 1.0) ; Верх лево
           (glVertex3f -1.0 -1.0 1.0) ; Низ лево
           (glVertex3f  1.0 -1.0 1.0) ; Низ право

           (glColor3f   1.0  1.0  0.0) ; Желтый
           (glVertex3f  1.0 -1.0 -1.0) ; Верх право квадрата (Зад)
           (glVertex3f -1.0 -1.0 -1.0) ; Верх лево
           (glVertex3f -1.0  1.0 -1.0) ; Низ лево
           (glVertex3f  1.0  1.0 -1.0) ; Низ право

           (glColor3f   0.0  0.0  1.0) ; Синий
           (glVertex3f -1.0  1.0  1.0) ; Верх право квадрата (Лево)
           (glVertex3f -1.0  1.0 -1.0) ; Верх лево
           (glVertex3f -1.0 -1.0 -1.0) ; Низ лево
           (glVertex3f -1.0 -1.0  1.0) ; Низ право

           (glColor3f  1.0  0.0  1.0) ; Фиолетовый
           (glVertex3f 1.0  1.0 -1.0) ; Верх право квадрата (Право)
           (glVertex3f 1.0  1.0  1.0) ; Верх лево
           (glVertex3f 1.0 -1.0  1.0) ; Низ лево
           (glVertex3f 1.0 -1.0 -1.0) ; Низ право
           )
         (glEnd)
         (swap-gl-buffers))))))

;
;                   TEST
;
(define-syntax-rule (run-lesson banner lesson-canvas)
  (let ([frame (new frame%
                    [label banner]
                    [width 600]
                    [height 400])])
    (new lesson-canvas
         [parent frame])
    (send frame show #t)))

(run-lesson "Lesson 5." lesson-5-canvas%)
