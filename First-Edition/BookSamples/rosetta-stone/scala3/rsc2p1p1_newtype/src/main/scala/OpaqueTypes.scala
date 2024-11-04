// Opaque types: zero-cost newtype approach

object OpaqueTypes:
  opaque type FirstName = String

  object FirstName:
    def apply(name: String): FirstName = name

    extension (name: FirstName)
      def toString: String = name


  opaque type LastName = String

  object LastName:
    def apply(name: String): LastName = name

    extension (name: LastName)
      def toString: String = name
