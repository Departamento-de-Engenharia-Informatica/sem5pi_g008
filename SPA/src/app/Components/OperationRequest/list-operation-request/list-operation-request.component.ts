import {Component, EventEmitter, OnInit, Output} from '@angular/core';
import {OperationRequestService} from '../../../services/OperationRequestService/operation-request.service';
import {OperationRequest} from '../../../Domain/OperationRequest';
import {Router} from '@angular/router';
import {PatientProfile} from '../../../Domain/PatientProfile';

@Component({
  selector: 'app-list-operation-request',
  templateUrl: './list-operation-request.component.html',
  styleUrl: './list-operation-request.component.css',
  standalone: false
})
export class ListOperationRequestComponent  {

  searchFilters = {
    patientName: null,
    operationType: null,
    priority: null,
    status: null
  };

  searchResults: any[] = [];


  constructor(private requisitionService: OperationRequestService, private router: Router) {}

  onSearch(): void {
    this.requisitionService.searchRequests(this.searchFilters).subscribe({
      next: (data) => {
        this.searchResults = data;
        console.log('Search Results:', this.searchResults);
      },
      error: (err) => {
        console.error('Error fetching data:', err);
      }
    });
  }
  deleteRequest(index: number): void {
    const requestToDeleteId = this.searchResults[index].id.value;


    const confirmDelete = window.confirm('Are you sure you want to delete this operation requisition?');

    if (confirmDelete) {
      this.requisitionService.deleteOperationRequest(requestToDeleteId).subscribe({
        next: () => {
          this.searchResults.splice(index, 1);
          console.log('Request deleted successfully.');
        },
        error: (err) => {
          console.error('Error deleting request:', err);
        }
      });
    } else {
      console.log('Delete action cancelled.');
    }
  }

  public editRequest(operation: MouseEvent){
    this.router.navigate(['staff/operationRequest/edit'], { state: { operation: operation } });
  }
  hasStatusColumn(): boolean {
    return this.searchResults.some(result => result.status);
  }

  public addOperationRequest() {
    this.router.navigate(['staff/operationRequest/add']);
  }

}
