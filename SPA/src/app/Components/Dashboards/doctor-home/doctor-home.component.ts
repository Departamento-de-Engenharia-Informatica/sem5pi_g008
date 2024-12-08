import {Component, Inject} from '@angular/core';
import {AuthService} from '../../../services/AuthService/auth.service';

@Component({
  selector: 'app-doctor-home',
  templateUrl: './doctor-home.component.html',
  styleUrls: ['./doctor-home.component.css'],
  standalone: false
})
export class DoctorMenuComponent {
  private auth: AuthService;

  menuItems = [
    /* The menus are temporary and will need to be replaced with the actual menu items
       but because they are not yet implemented I created some placeholders */
    { title: 'Operation Request Management', icon: 'assets/icons/operationRequest.png', link: '/staff/operationRequests' },
    { title: '3D',icon: 'assets/icons/3d.png', link: '/3d' }
  ];

  constructor(@Inject(AuthService) auth: AuthService) {
    this.auth = auth;
  }
}
