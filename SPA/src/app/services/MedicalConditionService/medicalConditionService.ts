import { Injectable } from '@angular/core';
import json from '../../appsettings.json';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { catchError, map, Observable, throwError } from 'rxjs';
import { MedicalConditionMapper } from '../../DTOs/mappers/medicalConditionMapper';
import { MedicalConditionDTO } from '../../DTOs/GenericDTOs/medicalConditionDTO';
import { BackendMedicalConditionDTO } from '../../DTOs/backendDTOs/backendMedicalConditionDTO';

@Injectable({
  providedIn: 'root',
})
export class MedicalConditionService {
  private apiUrl = `${json.backendApi['2'].url}/medicalConditions`;

  constructor(private http: HttpClient) {}

  addMedicalCondition(medicalConditionDTO: MedicalConditionDTO): Observable<string> {
    const medicalCondition = MedicalConditionMapper.dtoToDomain(medicalConditionDTO);
    const medicalConditionBackendDTO = MedicalConditionMapper.domainToBackendDto(medicalCondition);

    return this.http.post<string>(this.apiUrl, medicalConditionBackendDTO, { withCredentials: true }).pipe(
      catchError((error: HttpErrorResponse) => this.handleError(error))
    );
  }

  getAllMedicalConditions(): Observable<any[]> {
    return this.http.get<any>(this.apiUrl, { withCredentials: true }).pipe(
      map((response: any) => {
        if (response && Array.isArray(response.medicalConditions)) {
          return response.medicalConditions.map((backendDto: BackendMedicalConditionDTO) => {
            const domainModel = MedicalConditionMapper.backendDtoToDomain(backendDto);
            return MedicalConditionMapper.domainToDisplayDto(domainModel);
          });
        } else {
          throw new TypeError('medicalConditions is not an array or is missing');
        }
      }),
      catchError((error: HttpErrorResponse) => this.handleError(error))
    );
  }

  editMedicalCondition(originalMedicalConditionDTO: MedicalConditionDTO, updatedMedicalConditionDTO: MedicalConditionDTO): void {
   console.log("Not implemented yet.");
  }

  private handleError(error: HttpErrorResponse): Observable<never> {
    let errorMessage = 'An unknown error occurred.';

    if (error.status >= 805 && error.status <= 809) {
      errorMessage = error.error?.message || 'Backend-specific error occurred.';
    } else if (error.status === 500) {
      errorMessage = error.error?.message || 'Internal server error.';
    }

    console.error('HTTP Error:', error);
    return throwError(() => new Error(errorMessage));
  }
}
