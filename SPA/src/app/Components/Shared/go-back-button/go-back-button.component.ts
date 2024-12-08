import {Component, Input} from '@angular/core';
import {Router} from '@angular/router';
import {AuthService} from '../../../services/AuthService/auth.service';
import {switchMap} from 'rxjs';

@Component({
  selector: 'app-go-back-button',
  templateUrl: './go-back-button.component.html',
  styleUrls: ['./go-back-button.component.css'],
  standalone: false
})

export class GoBackButtonComponent {

  @Input() goBackPath: string = '';

  constructor(private router: Router, private auth: AuthService) {
  }

  goBack() {
    console.log('Go back path:', this.goBackPath);
    if(this.goBackPath) {
      this.router.navigate([this.goBackPath]);
    }else {
      this.navigateHome();
    }
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
