import React from 'react';
import './Grid.css';

const Grid: React.FC = () => {
  // Define the initial state of the grid
  const gridSize = 9;
  const gliderPattern = [
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 1, 1, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
  ];

  // Render the grid
  return (
    <div className="grid">
      {gliderPattern.map((row, rowIndex) => (
        <div key={rowIndex} className="grid-row">
          {row.map((cell, cellIndex) => (
            <div
              key={cellIndex}
              className={`grid-cell ${cell === 1 ? 'alive' : ''}`}
            />
          ))}
        </div>
      ))}
    </div>
  );
};

export default Grid;
