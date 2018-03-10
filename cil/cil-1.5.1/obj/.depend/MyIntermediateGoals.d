    $(OBJDIR)/MyIntermediateGoals.cmo:   $(OBJDIR)/usedef.cmo \
        $(OBJDIR)/MyUseDefAssocByHand.cmo \
        $(OBJDIR)/MyIfConditionMap.cmo \
        $(OBJDIR)/MyCriticalEdge.cmo \
        $(OBJDIR)/MyCilUtility.cmo \
        $(OBJDIR)/IntermediateGoals.cmo \
        $(OBJDIR)/Instruction.cmo     $(OBJDIR)/FindCil.cmo \
     $(OBJDIR)/errormsg.cmi  $(OBJDIR)/cil.cmi
    $(OBJDIR)/MyIntermediateGoals.cmx:   $(OBJDIR)/usedef.cmx \
        $(OBJDIR)/MyUseDefAssocByHand.cmx \
        $(OBJDIR)/MyIfConditionMap.cmx \
        $(OBJDIR)/MyCriticalEdge.cmx \
        $(OBJDIR)/MyCilUtility.cmx \
        $(OBJDIR)/IntermediateGoals.cmx \
        $(OBJDIR)/Instruction.cmx     $(OBJDIR)/FindCil.cmx \
     $(OBJDIR)/errormsg.cmx  $(OBJDIR)/cil.cmx
