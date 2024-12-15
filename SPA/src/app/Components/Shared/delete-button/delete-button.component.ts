import {Component, EventEmitter, Input, Output} from '@angular/core';

@Component({
  selector: 'app-delete-button',
  templateUrl: './delete-button.component.html',
  styleUrl: './delete-button.component.css',
  standalone: false
})

export class DeleteButtonComponent {
  @Input() staffId!: string;
  @Output() delete = new EventEmitter<string>();

  onDelete() {
    this.delete.emit(this.staffId);
  }
}
