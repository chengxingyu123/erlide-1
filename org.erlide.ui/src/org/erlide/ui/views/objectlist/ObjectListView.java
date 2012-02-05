//TODO: create an object, edit object in a seperate view

package org.erlide.ui.views.objectlist;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IErlideBackendVisitor;
import org.erlide.backend.events.ErlangEventHandler;
import org.erlide.jinterface.rpc.IRpcCallSite;
import org.erlide.ui.views.BackendContentProvider;
import org.erlide.ui.views.BackendLabelProvider;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpConverter;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.siteview.object.ErlObject;
import com.siteview.object.ErlObjectStore;

/**
 * 
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class ObjectListView extends ViewPart {

    public static final String ID = "org.erlide.ui.views.objectlist.ObjectListView";
    private ComboViewer backends;
    TableViewer viewer;
    private Action refreshAction;
    private Action deleteAction;
    Action doubleClickAction;

    /*
     * The content provider class is responsible for providing objects to the
     * view. It can wrap existing objects in adapters or simply return objects
     * as-is. These objects may be sensitive to the current input of the view,
     * or ignore it and always show the same content (like Task List, for
     * example).
     */
    class ViewContentProvider implements IStructuredContentProvider {

        private final ObjectEventHandler handler = new ObjectEventHandler(
                getBackend());

        public ViewContentProvider() {
            handler.register();
        }

        @Override
        public void inputChanged(final Viewer v, final Object oldInput,
                final Object newInput) {
        }

        @Override
        public void dispose() {
            final IBackend backend = getBackend();
        }

        @Override
        public Object[] getElements(final Object parent) {

            List<ErlObject> erlObjList = new ArrayList<ErlObject>(); 
            try {
                if (ErlObjectStore.size() == 0) return null;
                erlObjList = (List<ErlObject>) ErlObjectStore.get_all();
            } catch (Exception e) {
                e.printStackTrace();
            }
            
            final Object[] sss = new Object[erlObjList.size()];
            
            for (int i = 0; i < erlObjList.size(); i++) {
                sss[i] = erlObjList.get(i);
            }
            return sss;
        }

        class ObjectEventHandler extends ErlangEventHandler {

            public ObjectEventHandler(final IBackend backend) {
                super("objectlist", backend);
            }

            @Override
            public void handleEvent(final Event event) {
                Display.getDefault().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        if (!viewer.getControl().isDisposed()) {
                            viewer.refresh();
                        }
                    }
                });
            }
        }
    }

    static class ViewLabelProvider extends LabelProvider implements
            ITableLabelProvider {

        @Override
        public String getColumnText(final Object obj, final int index) {
            final ErlObject erlObj = (ErlObject) obj ;
            try {                    
                    if (index == 0) return erlObj.getExecutor().toString();
                    if (index == 1) return erlObj.getName();
                    if (index == 2) return erlObj.getType();
                    if (index == 3) return erlObj.getMem().toString();
                    if (index == 4) return erlObj.get_defined_attrs().toString();
                    if (index == 5) return erlObj.getState().toString();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            return null;
        }

        @Override
        public Image getColumnImage(final Object obj, final int index) {
            if (index == 0) {
                return getImage(obj);
            }
            return null;
        }

        @Override
        public Image getImage(final Object obj) {
            return PlatformUI.getWorkbench().getSharedImages()
                    .getImage(ISharedImages.IMG_OBJ_ELEMENT);
        }
    }

    /**
     * The constructor.
     */
    public ObjectListView() {
    }

    /**
     * This is a callback that will allow us to create the viewer and initialize
     * it.
     */
    @Override
    public void createPartControl(final Composite parent) {
        final Composite container = new Composite(parent, SWT.NONE);
        final GridLayout thisLayout = new GridLayout(2, false);
        container.setLayout(thisLayout);
        thisLayout.marginWidth = 5;
        thisLayout.marginHeight = 5;
        thisLayout.makeColumnsEqualWidth = false;
        thisLayout.verticalSpacing = 1;

        final Label label = new Label(container, SWT.SHADOW_NONE);
        label.setText("Erlang backend node");

        backends = new ComboViewer(container, SWT.SINGLE | SWT.V_SCROLL);
        final Combo combo = backends.getCombo();
        combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1,
                1));
        backends.getControl().setSize(
                new org.eclipse.swt.graphics.Point(319, 18));
        backends.setContentProvider(new BackendContentProvider());
        backends.setLabelProvider(new BackendLabelProvider());
        backends.setInput(BackendCore.getBackendManager());
        viewer = new TableViewer(container, SWT.SINGLE | SWT.V_SCROLL
                | SWT.FULL_SELECTION);
        final Table table = viewer.getTable();
        final GridData layoutData = new GridData(SWT.FILL, SWT.FILL, false,
                true, 2, 1);
        table.setLayoutData(layoutData);
        final Table t = (Table) viewer.getControl();
        final TableColumn colPid = new TableColumn(t, SWT.LEAD);
        colPid.setText("Executor");
        colPid.setWidth(150);
        final TableColumn colName = new TableColumn(t, SWT.LEAD);
        colName.setText("Name");
        colName.setWidth(300);
        final TableColumn colClass = new TableColumn(t, SWT.LEAD);
        colClass.setText("Class");
        colClass.setWidth(80);
        final TableColumn colMem = new TableColumn(t, SWT.LEAD);
        colMem.setText("Mem");
        colMem.setWidth(80);
        final TableColumn colAttrs = new TableColumn(t, SWT.LEAD);
        colAttrs.setText("Attrs");
        colAttrs.setWidth(60);
        final TableColumn colState = new TableColumn(t, SWT.LEAD);
        colState.setText("State");
        colState.setWidth(60);
        viewer.setContentProvider(new ViewContentProvider());
        viewer.setLabelProvider(new ViewLabelProvider());
        // viewer.setSorter(new NameSorter());
        viewer.setInput(getViewSite());
        viewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(final DoubleClickEvent event) {
                doubleClickAction.run();
            }
        });

        t.setLinesVisible(true);
        t.setHeaderVisible(true);

        // TODO this is wrong - all backends should be inited
        final IRpcCallSite ideBackend = BackendCore.getBackendManager()
                .getIdeBackend();
        if (ideBackend != null) {
            Objectlist.objectListInit(ideBackend);
        }
        BackendCore.getBackendManager().forEachBackend(
                new IErlideBackendVisitor() {
                    @Override
                    public void visit(final IBackend b) {
                        Objectlist.objectListInit(b);
                    }
                });

        makeActions();
        hookContextMenu();
        hookDoubleClickAction();
        contributeToActionBars();
    }

    private void hookContextMenu() {
        final MenuManager menuMgr = new MenuManager("#PopupMenu");
        final Menu menu = menuMgr.createContextMenu(viewer.getControl());
        viewer.getControl().setMenu(menu);
        getSite().registerContextMenu(menuMgr, viewer);
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(final IMenuManager manager) {
                ObjectListView.this.fillContextMenu(manager);
            }
        });
    }

    private void contributeToActionBars() {
        final IActionBars bars = getViewSite().getActionBars();
        fillLocalPullDown(bars.getMenuManager());
        fillLocalToolBar(bars.getToolBarManager());
    }

    private void fillLocalPullDown(final IMenuManager manager) {
        manager.add(refreshAction);
        manager.add(new Separator());
    }

    void fillContextMenu(final IMenuManager manager) {
        manager.add(refreshAction);
        manager.add(deleteAction);
        // Other plug-ins can contribute there actions here
        manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
    }

    private void fillLocalToolBar(final IToolBarManager manager) {
        manager.add(refreshAction);
    }

    private void makeActions() {
        refreshAction = new Action() {

            @Override
            public void run() {
                viewer.refresh();
            }
        };
        refreshAction.setText("Refresh");
        refreshAction.setToolTipText("Refresh object list");
        refreshAction.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
        
        deleteAction = new Action() {

            @Override
            public void run() {
                final ISelection selection = viewer.getSelection();
                final ErlObject obj = (ErlObject) ((IStructuredSelection) selection)
                        .getFirstElement();

                if (obj == null) {
                    return;
                }
                
                try {
                    ErlObjectStore.delete(obj);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                
                viewer.refresh();
            }
        };
        deleteAction.setText("Delete");
        deleteAction.setToolTipText("Delete this object");
        deleteAction.setImageDescriptor(PlatformUI.getWorkbench()
                .getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));

        doubleClickAction = new Action() {

            @Override
            public void run() {
                final ISelection selection = viewer.getSelection();
                final ErlObject obj = (ErlObject) ((IStructuredSelection) selection)
                        .getFirstElement();

                if (obj == null) {
                    return;
                }
                
                MessageDialog.openInformation(viewer.getControl().getShell(),
                        "Object detailed view", obj.getName());

 

            }
        };
    }

    private void hookDoubleClickAction() {
    }

    void showMessage(final String message) {
        MessageDialog.openInformation(viewer.getControl().getShell(),
                "Object list view", message);
    }

    /**
     * Passing the focus request to the viewer's control.
     */
    @Override
    public void setFocus() {
        viewer.getControl().setFocus();
    }

    public IBackend getBackend() {
        final IStructuredSelection sel = (IStructuredSelection) backends
                .getSelection();
        if (sel.getFirstElement() != null) {
            final IBackend b = (IBackend) sel.getFirstElement();
            return b;
        }
        final IBackend b = BackendCore.getBackendManager().getIdeBackend();
        if (b != null) {
            backends.setSelection(new StructuredSelection(b));
            return b;
        }
        return null;

    }

}
