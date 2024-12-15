import {Component, EventEmitter, Input, Output} from '@angular/core';
import {Specialization} from '../../../Domain/Specialization';

@Component({
  selector: 'app-specialization-list',
  templateUrl: './specialization-list.component.html',
  styleUrl: './specialization-list.component.css'
})
export class SpecializationListComponent {
  @Input() specializationList: Specialization[] = [];
   @Output() deleteSpecialization = new EventEmitter<string>();
  // @Output() editSpecialization = new EventEmitter<Specialization>();

}
