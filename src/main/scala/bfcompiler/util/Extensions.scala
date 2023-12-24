package bfcompiler.util

object Extensions {
  extension [A, B] (xs: List[Either[A, B]])
    def sequence: Either[List[A], List[B]] =
      xs.partitionMap(identity) match
        case (Nil, values) => Right(values)
        case (errors, _) => Left(errors)
}
