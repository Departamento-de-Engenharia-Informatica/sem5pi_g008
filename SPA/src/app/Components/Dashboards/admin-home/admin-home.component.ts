import {Component, Inject} from '@angular/core';
import {AuthService} from '../../../services/AuthService/auth.service';
import {Router} from '@angular/router';

@Component({
  selector: 'app-admin-home',
  templateUrl: './admin-home.component.html',
  styleUrls: ['./admin-home.component.css'],
  standalone: false
})
export class AdminMenuComponent {
  private auth: AuthService;

  menuItems = [
    {title: 'Operation Type Management', icon: 'assets/icons/dashboard.png', link: '/admin/operationTypeManagement'},
    {title: 'Staff Management', icon: 'assets/icons/users.png', link: '/admin/staff'},
    {title: 'Patient Management', icon: 'assets/icons/patient.png', link: '/admin/patient'},
    {title: 'Delete Pendent Users', icon: 'assets/icons/patient.png', link: '/patient/checkUserToDelete'},
    {title: '3D', icon: 'assets/icons/3d.png', link: '/3d'},
    {title: 'Specialization Management', icon: 'assets/icons/dashboard.png', link: '/admin/specialization'},
    {title: 'Allergy Management', icon: 'assets/icons/dashboard.png', link: '/admin/allergyManagement'}
  ];

  constructor(@Inject(AuthService) auth: AuthService) {
    this.auth = auth;
  }
}

