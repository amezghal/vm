package vm

type Value interface {
	int | float64 | string | VMValue | any | vmLocals
}

type Stack[T Value] []T

func (ss *Stack[T]) Push(value T) {
	*ss = append(*ss, value)
}

func (ss *Stack[T]) Pop() T {
	last := (*ss)[len(*ss)-1]
	*ss = (*ss)[:len(*ss)-1]
	return last
}

func (ss *Stack[T]) Tail() T {
	return (*ss)[len(*ss)-1]
}

func (ss *Stack[T]) AtIndex(index int) T {
	return (*ss)[index]
}

func (ss *Stack[T]) Len() int {
	return len(*ss)
}
