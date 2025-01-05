import {Component} from '@angular/core';
import {Router} from '@angular/router';
import {AuthService} from '../../../services/AuthService/auth.service';
import {switchMap} from 'rxjs';

@Component({
  selector: 'app-home-button',
  templateUrl: './home-button.component.html',
  styleUrls: ['./home-button.component.css'],
  standalone: false
})

export class HomeButtonComponent {
  constructor(private router: Router, private auth: AuthService) {
  }

  navigateHome() {
    this.auth.getUserRole().pipe(
      switchMap(role => {
        switch (role) {
          case 'Admin':
            return this.router.navigate(['/admin']);
          case 'Patient':
            return this.router.navigate(['/patient']);
          case 'Unregistered':
            return this.router.navigate(['/unregistered']);
          default:
            return this.router.navigate(['/staff']);
        }
      })
    ).subscribe();
  }
}
