import {Component, Inject} from '@angular/core';
import {AuthService} from '../../../services/AuthService/auth.service';

@Component({
  selector: 'app-staff-home',
  templateUrl: './staff-home.component.html',
  styleUrls: ['./staff-home.component.css'],
  standalone: false
})
export class StaffHomeComponent {
  private auth: AuthService;
  public menuItems: { title: string; icon: string; link: string }[] = [];
  dashboardTitle: string = 'Dashboard';

  constructor(@Inject(AuthService) auth: AuthService) {
    this.auth = auth;
  }

  ngOnInit(): void {
    this.auth.getUserRole().subscribe((role) => {
      if (role) {
        this.dashboardTitle = this.getTitleForRole(role);
        this.menuItems = this.getMenuItemsForRole(role);
      } else {
        console.error('No role found for the user.');
      }
    });
  }

  private getTitleForRole(role: string): string {
    switch (role.toLowerCase()) {
      case 'doctor':
        return 'Doctor Dashboard';
      case 'nurse':
        return 'Nurse Dashboard';
      case 'admin':
        return 'Admin Dashboard';
      default:
        return 'Dashboard';
    }
  }

  private getMenuItemsForRole(role: string): { title: string; icon: string; link: string }[] {
    const commonMenuItems = [
      { title: '3D', icon: 'assets/icons/3d.png', link: '/3d' },
    ];

    switch (role.toLowerCase()) {
      case 'doctor':
        return [
          { title: 'Operation Request Management', icon: 'assets/icons/operationRequest.png', link: '/staff/operationRequests' },
          { title: 'Patient Management', icon: 'assets/icons/patient.png', link: '/staff/patients' },
          { title: 'Update Appointment', icon: '', link: '/staff/appointment/update' },
          ...commonMenuItems,
        ];
      case 'nurse':
        return [
          { title: 'Patient Care Management', icon: 'assets/icons/patientCare.png', link: '/nurse/patientCare' },
          ...commonMenuItems,
        ];
      default:
        return commonMenuItems;
    }
  }
}
