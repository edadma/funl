package xyz.hyperreal.bvm

import xyz.hyperreal.dal.Type

case class VMInterface(cons: (VMObject, VMList) => VMCons,
                       nil: VMNil,
                       number: ((Type, Number)) => VMNumber,
                       range: (Number, Number, Number, Boolean) => VMIterable)
