package uvm.clientsupport.text;

import static org.junit.Assert.*;

import org.junit.Test;

public class MuIRBuilderTest {

    @Test
    public void nameFactoryTest() {
        String prefix1 = "@mylang.type";
        NameFactory nf1 = new NameFactory(prefix1);
        String prefix2 = "@yourlang.func";
        NameFactory nf2 = new NameFactory(prefix2);

        String name11 = nf1.nextName("int32");
        String name21 = nf2.nextName("append");
        String name12 = nf1.nextName("StringBuilder");
        String name22 = nf2.nextName();

        assertNotEquals(name11, name12);
        assertNotEquals(name11, name21);
        assertNotEquals(name21, name12);
        assertNotEquals(name21, name22);

        assertStartsWith(name11, prefix1);
        assertStartsWith(name12, prefix1);
        assertStartsWith(name21, prefix2);
        assertStartsWith(name22, prefix2);
    }

    public void assertStartsWith(String main, String prefix) {
        assertTrue(String.format("%s does not start with %s", main, prefix), main.startsWith(prefix));
    }

    public void assertEndsWith(String main, String suffix) {
        assertTrue(String.format("%s does not end with %s", main, suffix), main.endsWith(suffix));
    }
    
    @Test
    public void funcBuildingTest() {
        FuncDefBuilder b = new FuncDefBuilder("@mylang.internal.add", "@mylang.internal.add.v1", "@foosig");
        String xMuName = b.addParam("x");
        String yMN = b.addParam("y");
        
        InstBinOp add = new InstBinOp();
        add.setName(b.nextInstName("add_x_y"));
        assertEndsWith(add.getName(), "add_x_y");
        
        add.setOp("ADD");
        add.setOpndTy("@i32");
        add.setOp1(xMuName);
        add.setOp2(yMN);
        b.emit(add);
        
        FuncDef fd = b.getFuncDef();
        BasicBlock entry = fd.getBbs().get(0);
        
        assertEndsWith(entry.getName(), "entry");
        
        Instruction inst0 = entry.getInsts().get(0);
        assertEquals(add, inst0);
        
        BasicBlock bb2 = b.newBB("nextBlock");
        assertTrue(fd.getBbs().contains(bb2));
    
        InstBranch br = new InstBranch();
        br.setName(b.nextInstName("initial_jump"));
        br.setDest(bb2.getName());
        
        b.emit(br);
        assertTrue(entry.getInsts().contains(br));

        b.setCurBB(bb2);
        
        InstBinOp sub = new InstBinOp();
        sub.setName(b.nextInstName("sub"));
        sub.setOp("SUB");
        sub.setOpndTy("@i32");
        sub.setOp1(xMuName);
        sub.setOp2(yMN);
        b.emit(sub);
        
        assertFalse(entry.getInsts().contains(sub));
        assertTrue(bb2.getInsts().contains(sub));
        
        System.out.println(TextIRWriter.funcDefToText(fd));
    }
    
    @Test
    public void customBuilderTest() {
        class CustomFuncDefBuilder extends FuncDefBuilder {
            public final String xMuName;
            public final String yMuName;
            
            public CustomFuncDefBuilder(String name) {
                super(name, name+".v1", "@ThisKindOfFunctionAlwaysHasThisSignature");
                xMuName = addParam("x");
                yMuName = addParam("y");
            }
            
            public String emitAddI32(String hint, String op1, String op2) {
                InstBinOp add = new InstBinOp();
                String name = nextInstName(hint);
                add.setName(name);
                add.setOp("ADD");   // because this function is dedicated to adding.
                add.setOpndTy("@i32"); // because this function is dedicated to i32.
                add.setOp1(op1);
                add.setOp2(op2);
                emit(add);
                return name;
            }
            
            public void emitJumpIfNegative(String hint, String opnd, String dest) {
                InstCmp lt = new InstCmp();
                String ltName = nextInstName(hint+"_compare");
                lt.setName(ltName);
                lt.setOp("SLT");
                lt.setOpndTy("@i32");
                lt.setOp1(opnd);
                lt.setOp2("@CONST_I32_NEG_1"); // Please define this constant at the top level.
                emit(lt);
                
                InstBranch2 br2 = new InstBranch2();
                String br2Name = nextInstName(hint+"_branch");
                br2.setName(br2Name);
                br2.setCond(ltName);
                br2.setIfTrue(dest);
                
                BasicBlock bbNew = newBB(hint+"_cont");
                br2.setIfFalse(bbNew.getName());
                emit(br2);
                
                setCurBB(bbNew);    // Behave as if it is a straight-line code, but it has just branched.
            }
        }
        
       CustomFuncDefBuilder b = new CustomFuncDefBuilder("myAdder");
       String xMN = b.xMuName;
       String yMN = b.yMuName;
       String zMN = b.emitAddI32("add", xMN, yMN);
       
       BasicBlock excHandler = b.newBB("handle_negative");
       
       b.emitJumpIfNegative("guard_positive", zMN, excHandler.getName());
       
       FuncDef fd = b.getFuncDef();
       
       BasicBlock entry = b.getEntry();
       
       assertTrue(entry.getInsts().get(0) instanceof InstBinOp);
       assertTrue(entry.getInsts().get(1) instanceof InstCmp);
       assertTrue(entry.getInsts().get(2) instanceof InstBranch2);
       
       System.out.println(TextIRWriter.funcDefToText(fd));
    }

}
