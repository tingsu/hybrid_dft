    $(OBJDIR)/MyUseDefAssocByHand.cmo:   $(OBJDIR)/usedef.cmo \
        $(OBJDIR)/MyIfConditionMap.cmo \
        $(OBJDIR)/MyDfSetting.cmo \
        $(OBJDIR)/MyCriticalEdge.cmo \
        $(OBJDIR)/MyCilUtility.cmo \
        $(OBJDIR)/Instruction.cmo     $(OBJDIR)/FindCil.cmo \
     $(OBJDIR)/errormsg.cmi     $(OBJDIR)/DistanceToTargets.cmo \
        $(OBJDIR)/CilCallgraph.cmo  $(OBJDIR)/cil.cmi
    $(OBJDIR)/MyUseDefAssocByHand.cmx:   $(OBJDIR)/usedef.cmx \
        $(OBJDIR)/MyIfConditionMap.cmx \
        $(OBJDIR)/MyDfSetting.cmx \
        $(OBJDIR)/MyCriticalEdge.cmx \
        $(OBJDIR)/MyCilUtility.cmx \
        $(OBJDIR)/Instruction.cmx     $(OBJDIR)/FindCil.cmx \
     $(OBJDIR)/errormsg.cmx     $(OBJDIR)/DistanceToTargets.cmx \
        $(OBJDIR)/CilCallgraph.cmx  $(OBJDIR)/cil.cmx
