class XmlSerializer:
    def __init__( self ):
        self.depth = 0

    def serialize( self, obj ):
        result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        self.depth = 0

        if hasattr( obj, "__class__" ):
            result += self.__serialize_class( obj )

        return result

    def __serialize_class( self, obj ):
        result = ""
        node_name = obj.__class__.__name__
        node_attributes = {}
        node_content = ""

        # go through class properties
        for prop in vars( obj ):

            prop_value = getattr( obj,prop )

            # List - there should be only one list of elements as a content
            if isinstance( prop_value, list ):
                self.depth += 1
                for item in prop_value:
                    node_content += self.__serialize_class( item )
                self.depth -= 1

            # Dictionary - appended to attributes
            elif isinstance( prop_value, dict ):
                node_attributes = { **node_attributes, **prop_value}

            # Complex sub objects, serialized as sub nodes
            elif not self.__is_primitive( prop_value ):
                self.depth += 1
                node_content += self.__serialize_class( prop_value )
                self.depth -= 1

            # Primitives, serialized as attributes
            else:
                node_attributes[prop] = prop_value

        # node opening <node>
        result += "\t" * self.depth + "<%(n)s" % {"n": node_name}

        # node attributes
        for k, v in node_attributes.items():
            result += " %(k)s=\"%(v)s\"" % {"k": k, "v": v}

        if len( node_content ) > 0:
            result += ">\n"

            # node content
            result += node_content

            # node closing </node>
            result += "\t" * self.depth + "</%(n)s>\n" % {"n": node_name}
        else:
            result += "/>\n"

        return result

    def __is_primitive( self, obj ):
        return isinstance( obj, (int, float, str, bool) )
